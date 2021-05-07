{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Server.Handler where

import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
import Control.Exception (evaluate)
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.Massiv.Array as Massiv
import qualified Data.Massiv.Array.Mutable as Massiv.Mutable
import qualified Data.OrdPSQ as OrdPSQ
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified Graphics.Image as HIP
import qualified Hasql.Pool
import qualified Hasql.Session as Hasql
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..))
import qualified Hasql.Transaction.Sessions as Transaction
import Network.Wai.EventSource
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
import qualified Positive.Image as Image
import qualified Positive.Image.Util as Util
import qualified Positive.Language as Language
import qualified Positive.Log as Log
import Positive.Prelude hiding (ByteString)
import qualified Positive.SingleImage as SingleImage
import Servant
import qualified System.Directory as Directory
import System.FilePath.Posix (isPathSeparator, (</>))

type PositiveT m =
  ReaderT Env m

data Env = Env
  { imageMVar :: !(MVar (OrdPSQ Text UTCTime (ImageCrop, Image.Monochrome))),
    previewMVar :: !(MVar ()),
    eventChan :: !(Chan ServerEvent),
    isDev :: !CLI.IsDev,
    sqlPool :: !Hasql.Pool.Pool,
    logger :: !Log.TimedFastLogger
  }

-- IMAGE

handleImage :: Text -> ImageSettings -> PositiveT Handler ByteString
handleImage dir settings = do
  logSSE $ "Requested image " <> Path.unpack settings.filename
  env <- ask
  (image, putMVarBack) <-
    getCachedImage settings.crop $
      Text.pack (Text.unpack dir </> Path.toFilePath settings.filename)
  if env.isDev
    then do
      processed <- timed "Apply" $ Image.applySettings settings image
      encoded <- timed "Encode" =<< Image.encode "_.png" processed
      liftIO putMVarBack
      pure encoded
    else do
      log $ "Apply settings and encode: " <> Path.unpack settings.filename
      !encoded <- Image.encode "_.png" $ Image.applySettings settings image
      liftIO putMVarBack
      logSSE $ "Processed image " <> Path.unpack settings.filename
      pure encoded

-- SAVE

handleSaveFilmRoll :: FilmRoll -> PositiveT Handler FilmRoll
handleSaveFilmRoll filmRoll = do
  pool <- asks sqlPool
  _ <-
    liftIO . Hasql.Pool.use pool
      . Transaction.transaction Serializable Write
      $ Session.updateFilmRoll filmRoll
  logDebug "Wrote settings"
  env <- ask
  void . liftIO $ MVar.tryPutMVar env.previewMVar ()
  pure filmRoll

-- CHECK EXPRESSIONS

handleCheckExpressions :: [Expression] -> PositiveT Handler [ExpressionResult]
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

handleGenerateHighRes :: ImageSettings -> PositiveT Handler NoContent
handleGenerateHighRes settings = do
  filmRoll <- runSession $ Session.selectFilmRollByImageSettings settings.id
  let input = Path.toFilePath filmRoll.directoryPath </> Path.toFilePath settings.filename
  env <- ask
  SingleImage.generate (Log.log env.logger) "Generating highres version: " input settings
  pure NoContent

handleGenerateWallpaper :: ImageSettings -> PositiveT Handler NoContent
handleGenerateWallpaper settings = do
  filmRoll <- runSession $ Session.selectFilmRollByImageSettings settings.id
  let input = Path.toFilePath filmRoll.directoryPath </> Path.toFilePath settings.filename
      outputBase homeDir =
        homeDir
          </> "Documents/wallpapers/positive"
          </> filter (\c -> not (isPathSeparator c || c == '.')) (Path.toFilePath filmRoll.directoryPath)
          <> " | "
          <> Path.toFilePath settings.filename
  log $ "Generating wallpaper version of: " <> Text.pack input
  output <- Util.ensureUniqueFilename . outputBase =<< liftIO Directory.getHomeDirectory
  image <-
    handleLeft $
      Image.fromDiskPreProcess (Just 2560) settings.crop input
  HIP.writeImage output $ Image.applySettings settings image
  NoContent <$ log ("Wrote wallpaper version of: " <> Text.pack input)

-- OPEN EXTERNALEDITOR

handleOpenExternalEditor :: ImageSettings -> PositiveT Handler NoContent
handleOpenExternalEditor settings = do
  filmRoll <- runSession $ Session.selectFilmRollByImageSettings settings.id
  let input = Path.toFilePath filmRoll.directoryPath </> Path.toFilePath settings.filename
  log $ "Opening in external editor: " <> Text.pack input
  image <-
    handleLeft $
      Image.fromDiskPreProcess Nothing settings.crop input
  HIP.displayImageUsing HIP.defaultViewer False (Image.applySettings settings image)
  pure NoContent

-- HISTOGRAM

handleGetSettingsHistogram :: ImageSettings -> PositiveT Handler [Int]
handleGetSettingsHistogram settings =
  let toHistogram arr =
        Massiv.Mutable.createArrayST_ @Massiv.P @_ @Int
          (Massiv.Sz1 (1 + fromIntegral (maxBound :: Word8)))
          $ \marr ->
            Massiv.forM_ arr $
              \(HIP.PixelY p) -> Massiv.modify marr (pure . (+) 1) (fromIntegral (HIP.toWord8 p))
   in do
        filmRoll <- runSession $ Session.selectFilmRollByImageSettings settings.id
        (image, putMVarBack) <-
          getCachedImage settings.crop $
            Text.pack (Path.toFilePath filmRoll.directoryPath </> Path.toFilePath settings.filename)
        liftIO putMVarBack
        logDebug $ "Creating histogram for: " <> Path.unpack settings.filename
        pure . Massiv.toList . toHistogram . HIP.unImage $
          Image.applySettings settings image

-- COORDINATE

handleGetCoordinateInfo :: ([(Double, Double)], ImageSettings) -> PositiveT Handler [CoordinateInfo]
handleGetCoordinateInfo (coordinates, settings) =
  let toInfo image (x, y) =
        CoordinateInfo x y
          . (\(HIP.PixelY p) -> HIP.toDouble p)
          . HIP.borderIndex Massiv.Continue image
          $ HIP.Ix2
            (floor (int2Double (HIP.rows image) * y))
            (floor (int2Double (HIP.cols image) * x))
   in do
        filmRoll <- runSession $ Session.selectFilmRollByImageSettings settings.id
        (image, putMVarBack) <-
          first (Image.applySettings settings)
            <$> getCachedImage settings.crop
              ( Text.pack
                  ( Path.toFilePath filmRoll.directoryPath
                      </> Path.toFilePath settings.filename
                  )
              )
        liftIO putMVarBack
        pure $ fmap (toInfo image) coordinates

-- LIST DIRECTORIES

handleGetSettings :: PositiveT Handler [FilmRoll]
handleGetSettings =
  runSession Session.selectFilmRolls

-- HANDLER HELPERS

runSession :: Hasql.Session a -> PositiveT Handler a
runSession session = do
  pool <- asks sqlPool
  result <- liftIO $ Hasql.Pool.use pool session
  either (\err -> log (tshow err) >> throwError err500) pure result

handleLeft :: PositiveT Handler (Either err a) -> PositiveT Handler a
handleLeft m =
  m >>= either (\(_ :: err) -> log "Image read error" >> throwError err404) pure

-- | Read image from disk, normalize before crop, keep result in MVar
getCachedImage :: ImageCrop -> Text -> PositiveT Handler (Image.Monochrome, IO ())
getCachedImage crop path = do
  env <- ask
  now <- liftIO Time.getCurrentTime
  cache <- liftIO $ MVar.takeMVar env.imageMVar
  log $ "Cached images: " <> tshow (OrdPSQ.size cache)
  case checkCrop crop =<< OrdPSQ.lookup path cache of
    Just (_, cached@(_, loadedImage)) -> do
      logDebug "From cache"
      pure
        ( loadedImage,
          MVar.putMVar env.imageMVar
            =<< evaluate (DeepSeq.force (insertAndTrim path now cached cache))
        )
    Nothing -> do
      logDebug "From disk"
      image <-
        handleLeft $
          Image.fromDiskPreProcess (Just 1440) crop (Text.unpack path)
      pure
        ( image,
          MVar.putMVar env.imageMVar
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

log :: MonadIO m => Text -> PositiveT m ()
log !msg = do
  env <- ask
  liftIO . env.logger $ Log.format "info" msg

logDebug :: MonadIO m => Text -> PositiveT m ()
logDebug !msg = do
  env <- ask
  when env.isDev . liftIO . env.logger $ Log.format "debug" msg

logSSE :: MonadIO m => Text -> PositiveT m ()
logSSE !msg = do
  env <- ask
  liftIO . Chan.writeChan env.eventChan $
    ServerEvent (Just "log") Nothing [Builder.byteString $ encodeUtf8 msg]

-- PROFILE

timed :: Text -> a -> PositiveT Handler a
timed name action = do
  logDebug $ name <> " - started"
  start <- liftIO Time.getCurrentTime
  a <- liftIO $ evaluate action
  done <- liftIO Time.getCurrentTime
  logDebug $ name <> " - processed in: " <> tshow (Time.diffUTCTime done start)
  pure a
