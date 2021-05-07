{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Server.Handler where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
import Control.Effect.Labelled
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Effect.Throw
import Control.Exception (evaluate)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Massiv.Array as Massiv
import qualified Data.Massiv.Array.Mutable as Massiv.Mutable
import qualified Data.OrdPSQ as OrdPSQ
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified Graphics.Image as HIP
import qualified Positive.CLI as CLI
import Positive.Data.FilmRoll (FilmRoll)
import Positive.Data.ImageSettings
  ( CoordinateInfo (..),
    Expression (..),
    ExpressionResult (..),
    ImageCrop (..),
    ImageSettings (..),
  )
import qualified Positive.Data.Path as Path
import qualified Positive.Database.Session as Session
import Positive.Effect.Log
import Positive.Effect.PostgreSQL (PostgreSQL)
import qualified Positive.Effect.PostgreSQL as PostgreSQL
import qualified Positive.Image as Image
import qualified Positive.Image.Util as Util
import qualified Positive.Language as Language
import qualified Positive.Log as Log
import Positive.Prelude hiding (ByteString)
import qualified Positive.SingleImage as SingleImage
import Servant hiding (throwError)
import qualified System.Directory as Directory
import System.FilePath.Posix (isPathSeparator, (</>))

type PositiveT sig m =
  ( HasLabelled "stdout" Log sig m,
    HasLabelled "sse" Log sig m,
    Has PostgreSQL sig m,
    Has (Reader Env) sig m,
    Has (Lift IO) sig m,
    Has (Throw PostgreSQL.Error) sig m,
    Has (Throw ServerError) sig m
  )

data Env = Env
  { imageMVar :: !(MVar (OrdPSQ Text UTCTime (ImageCrop, Image.Monochrome))),
    previewMVar :: !(MVar ()),
    isDev :: !CLI.IsDev
  }

-- IMAGE

handleImage :: PositiveT sig m => Text -> ImageSettings -> m ByteString
handleImage dir settings = do
  logInfo @"sse" $ "Requested image " <> Path.unpack settings.filename
  evv <- ask @Env
  (image, putMVarBack) <-
    getCachedImage settings.crop $
      Text.pack (Text.unpack dir </> Path.toFilePath settings.filename)
  if evv.isDev
    then do
      processed <- timed "Apply" $ Image.applySettings settings image
      encoded <- timed "Encode" =<< sendIO (Image.encode "_.png" processed)
      sendIO putMVarBack
      pure encoded
    else do
      logInfo @"stdout" $ "Apply settings and encode: " <> Path.unpack settings.filename
      !encoded <- sendIO . Image.encode "_.png" $ Image.applySettings settings image
      sendIO putMVarBack
      logInfo @"sse" $ "Processed image " <> Path.unpack settings.filename
      pure encoded

-- SAVE

handleSaveFilmRoll :: PositiveT sig m => FilmRoll -> m FilmRoll
handleSaveFilmRoll filmRoll = do
  _ <- PostgreSQL.runTransaction $ Session.updateFilmRoll filmRoll
  logDebug @"stdout" "Wrote settings"
  evv <- ask @Env
  void . sendIO $ MVar.tryPutMVar evv.previewMVar ()
  pure filmRoll

-- CHECK EXPRESSIONS

handleCheckExpressions :: PositiveT sig m => [Expression] -> m [ExpressionResult]
handleCheckExpressions exprs =
  let eval v expr =
        SampleEval $
          filter
            (not . isInfinite)
            [Language.eval p v expr - p | p <- [0.1, 0.2 .. 1.0]]
      parseAndCheck e =
        first TypeError . Language.check
          =<< first SyntaxError (Language.parse e.eExpr)
   in pure [either identity (eval e.eValue) (parseAndCheck e) | e <- exprs]

-- GENERATE

handleGenerateHighRes :: PositiveT sig m => ImageSettings -> m NoContent
handleGenerateHighRes settings = do
  filmRoll <- PostgreSQL.runSession $ Session.selectFilmRollByImageSettings settings.id
  let input = Path.toFilePath filmRoll.directoryPath </> Path.toFilePath settings.filename
  -- evv <- ask @Env
  -- FIXME
  -- SingleImage.generate (Log.logInfo @"stdout" evv.logger) "Generating highres version: " input settings
  pure NoContent

handleGenerateWallpaper :: PositiveT sig m => ImageSettings -> m NoContent
handleGenerateWallpaper settings = do
  filmRoll <- PostgreSQL.runSession $ Session.selectFilmRollByImageSettings settings.id
  let input = Path.toFilePath filmRoll.directoryPath </> Path.toFilePath settings.filename
      outputBase homeDir =
        homeDir
          </> "Documents/wallpapers/positive"
          </> filter (\c -> not (isPathSeparator c || c == '.')) (Path.toFilePath filmRoll.directoryPath)
          <> " | "
          <> Path.toFilePath settings.filename
  logInfo @"stdout" $ "Generating wallpaper version of: " <> Text.pack input
  output <- sendIO $ Util.ensureUniqueFilename . outputBase =<< sendIO Directory.getHomeDirectory
  image <-
    handleLeft
      . sendIO
      $ Image.fromDiskPreProcess (Just 2560) settings.crop input
  sendIO . HIP.writeImage output $ Image.applySettings settings image
  NoContent <$ logInfo @"stdout" ("Wrote wallpaper version of: " <> Text.pack input)

-- OPEN EXTERNALEDITOR

handleOpenExternalEditor :: PositiveT sig m => ImageSettings -> m NoContent
handleOpenExternalEditor settings = do
  filmRoll <- PostgreSQL.runSession $ Session.selectFilmRollByImageSettings settings.id
  let input = Path.toFilePath filmRoll.directoryPath </> Path.toFilePath settings.filename
  logInfo @"stdout" $ "Opening in external editor: " <> Text.pack input
  image <-
    handleLeft
      . sendIO
      $ Image.fromDiskPreProcess Nothing settings.crop input
  sendIO $ HIP.displayImageUsing HIP.defaultViewer False (Image.applySettings settings image)
  pure NoContent

-- HISTOGRAM

handleGetSettingsHistogram :: PositiveT sig m => ImageSettings -> m [Int]
handleGetSettingsHistogram settings =
  let toHistogram arr =
        Massiv.Mutable.createArrayST_ @Massiv.P @_ @Int
          (Massiv.Sz1 (1 + fromIntegral (maxBound :: Word8)))
          $ \marr ->
            Massiv.forM_ arr $
              \(HIP.PixelY p) -> Massiv.modify marr (pure . (+) 1) (fromIntegral (HIP.toWord8 p))
   in do
        filmRoll <- PostgreSQL.runSession $ Session.selectFilmRollByImageSettings settings.id
        (image, putMVarBack) <-
          getCachedImage settings.crop $
            Text.pack (Path.toFilePath filmRoll.directoryPath </> Path.toFilePath settings.filename)
        sendIO putMVarBack
        logDebug @"stdout" $ "Creating histogram for: " <> Path.unpack settings.filename
        pure . Massiv.toList . toHistogram . HIP.unImage $
          Image.applySettings settings image

-- COORDINATE

handleGetCoordinateInfo :: PositiveT sig m => ([(Double, Double)], ImageSettings) -> m [CoordinateInfo]
handleGetCoordinateInfo (coordinates, settings) =
  let toInfo image (x, y) =
        CoordinateInfo x y
          . (\(HIP.PixelY p) -> HIP.toDouble p)
          . HIP.borderIndex Massiv.Continue image
          $ HIP.Ix2
            (floor (int2Double (HIP.rows image) * y))
            (floor (int2Double (HIP.cols image) * x))
   in do
        filmRoll <- PostgreSQL.runSession $ Session.selectFilmRollByImageSettings settings.id
        (image, putMVarBack) <-
          first (Image.applySettings settings)
            <$> getCachedImage settings.crop
              ( Text.pack
                  ( Path.toFilePath filmRoll.directoryPath
                      </> Path.toFilePath settings.filename
                  )
              )
        sendIO putMVarBack
        pure $ fmap (toInfo image) coordinates

-- LIST DIRECTORIES

handleGetSettings :: PositiveT sig m => m [FilmRoll]
handleGetSettings =
  PostgreSQL.runSession Session.selectFilmRolls

-- HANDLER HELPERS
handleLeft :: PositiveT sig m => m (Either err a) -> m a
handleLeft m =
  m >>= either (\(_ :: err) -> logInfo @"stdout" "Image read error" >> throwError err404) pure

-- | Read image from disk, normalize before crop, keep result in MVar
getCachedImage :: PositiveT sig m => ImageCrop -> Text -> m (Image.Monochrome, IO ())
getCachedImage crop path = do
  evv <- ask @Env
  now <- sendIO Time.getCurrentTime
  cache <- sendIO $ MVar.takeMVar evv.imageMVar
  logInfo @"stdout" $ "Cached images: " <> tshow (OrdPSQ.size cache)
  case checkCrop crop =<< OrdPSQ.lookup path cache of
    Just (_, cached@(_, loadedImage)) -> do
      logDebug @"stdout" "From cache"
      pure
        ( loadedImage,
          MVar.putMVar evv.imageMVar
            =<< evaluate (DeepSeq.force (insertAndTrim path now cached cache))
        )
    Nothing -> do
      logDebug @"stdout" "From disk"
      image <-
        handleLeft . sendIO $
          Image.fromDiskPreProcess (Just 1440) crop (Text.unpack path)
      pure
        ( image,
          MVar.putMVar evv.imageMVar
            =<< evaluate (DeepSeq.force (insertAndTrim path now (crop, image) cache))
        )

checkCrop ::
  ImageCrop ->
  (UTCTime, (ImageCrop, Image.Monochrome)) ->
  Maybe (UTCTime, (ImageCrop, Image.Monochrome))
checkCrop crop cached@(_, (cachedCrop, _))
  | crop == cachedCrop = Just cached
  | otherwise = Nothing

insertAndTrim :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
insertAndTrim k v p psq =
  let q = OrdPSQ.insert k v p psq
   in if OrdPSQ.size q > 40 then OrdPSQ.deleteMin q else q

-- LOG

-- logInfo @"stdout" :: MonadIO m => Text -> PositiveT m ()
-- logInfo @"stdout" !msg = do
--   evv <- ask @Env
--   sendIO . evv.logger $ Log.format "info" msg

-- logDebug :: MonadIO m => Text -> PositiveT m ()
-- logDebug !msg = do
--   evv <- ask @Env
--   when evv.isDev . sendIO . evv.logger $ Log.format "debug" msg

-- logInfo @"sse" :: MonadIO m => Text -> PositiveT m ()
-- logInfo @"sse" !msg = do
--   evv <- ask @Env
--   sendIO . Chan.writeChan evv.eventChan $
--     ServerEvent (Just "logInfo @"stdout"") Nothing [Builder.byteString $ encodeUtf8 msg]

-- PROFILE

timed :: PositiveT sig m => Text -> a -> m a
timed name action = do
  logDebug @"stdout" $ name <> " - started"
  start <- sendIO Time.getCurrentTime
  a <- sendIO $ evaluate action
  done <- sendIO Time.getCurrentTime
  logDebug @"stdout" $ name <> " - processed in: " <> tshow (Time.diffUTCTime done start)
  pure a
