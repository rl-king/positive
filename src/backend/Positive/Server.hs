{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Server
  ( run,
  )
where

import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
import Control.Exception (evaluate)
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Massiv.Array as Massiv
import qualified Data.Massiv.Array.Mutable as Massiv.Mutable
import qualified Data.OrdPSQ as OrdPSQ
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified Graphics.Image as HIP
import qualified Hasql.Pool
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..))
import qualified Hasql.Transaction.Sessions as Transaction
import Network.Wai.EventSource
import qualified Network.Wai.Handler.Warp as Warp
import Positive.Api
import qualified Positive.CLI as CLI
import Positive.Data.FilmRoll (FilmRoll)
import Positive.Data.ImageSettings
  ( CoordinateInfo (..),
    Expression (..),
    ExpressionResult (..),
    ImageCrop (..),
    ImageSettings (..),
  )
import qualified Positive.Database.Session as Session
import qualified Positive.Filename as Filename
import qualified Positive.Image as Image
import qualified Positive.Image.Util as Util
import qualified Positive.Language as Language
import qualified Positive.Log as Log
import Positive.Prelude hiding (ByteString)
import qualified Positive.Preview as Preview
import qualified Positive.SingleImage as SingleImage
import qualified Positive.Static as Static
import Servant
import Servant.Server.Generic
import qualified System.Directory as Directory
import System.FilePath.Posix (isPathSeparator, (</>))

-- POSITIVE

type PositiveT m =
  ReaderT Env m

data Env = Env
  { imageMVar :: !(MVar (OrdPSQ Text UTCTime (ImageCrop, Image.Monochrome))),
    previewMVar :: !(MVar [(FilePath, ImageSettings)]),
    eventChan :: !(Chan ServerEvent),
    isDev :: !CLI.IsDev,
    sqlPool :: !Hasql.Pool.Pool,
    logger :: !Log.TimedFastLogger
  }

-- SERVER

run :: Log.TimedFastLogger -> CLI.IsDev -> CLI.Port -> IO ()
run logger_ isDev_ port =
  let settings =
        Warp.setPort port $
          Warp.setBeforeMainLoop
            ( Log.log logger_ $
                Text.concat
                  ["listening on port: ", tshow port, ", is dev: ", tshow isDev_]
            )
            Warp.defaultSettings
   in do
        imageMVar_ <- MVar.newMVar OrdPSQ.empty
        previewMVar_ <- MVar.newEmptyMVar
        eventChan_ <- Chan.newChan
        pool <- Hasql.Pool.acquire (3, 10, "host=localhost port=5432 dbname=positive")
        let env = Env imageMVar_ previewMVar_ eventChan_ isDev_ pool logger_
        Preview.loop previewMVar_ imageMVar_ eventChan_ (Log.log logger_)
        Warp.runSettings settings $
          genericServeT (`runReaderT` env) (handlers isDev_ eventChan_)

-- HANDLERS

handlers :: CLI.IsDev -> Chan ServerEvent -> Api (AsServerT (PositiveT Handler))
handlers isDev_ chan =
  Api
    { aImageApi =
        genericServerT
          ImageApi
            { iaImage = handleImage,
              iaEvents = pure $ eventSourceAppChan chan,
              iaRaw = Static.serve isDev_
            },
      aSettingsApi =
        genericServerT
          SettingsApi
            { saSaveSettings = handleSaveSettings,
              saCheckExpressions = handleCheckExpressions,
              saGetSettings = handleGetSettings,
              saGetSettingsHistogram = handleGetSettingsHistogram,
              saGenerateHighRes = handleGenerateHighRes,
              saOpenExternalEditor = handleOpenExternalEditor,
              saGetCoordinateInfo = handleGetCoordinateInfo,
              saGenerateWallpaper = handleGenerateWallpaper
            }
    }

-- IMAGE

handleImage :: Text -> ImageSettings -> PositiveT Handler ByteString
handleImage dir settings = do
  logSSE $ "Requested image " <> Filename.toText settings.iFilename
  env <- ask
  (image, putMVarBack) <-
    getCachedImage settings.iCrop $
      Text.pack (Text.unpack dir </> Filename.toFilePath settings.iFilename)
  if env.isDev
    then do
      processed <- timed "Apply" $ Image.applySettings settings image
      encoded <- timed "Encode" =<< Image.encode "_.png" processed
      liftIO putMVarBack
      pure encoded
    else do
      log $ "Apply settings and encode: " <> Filename.toText settings.iFilename
      !encoded <- Image.encode "_.png" $ Image.applySettings settings image
      liftIO putMVarBack
      logSSE $ "Processed image " <> Filename.toText settings.iFilename
      pure encoded

-- SAVE

handleSaveSettings :: Text -> FilmRoll -> PositiveT Handler FilmRoll
handleSaveSettings dir newSettings = do
  pool <- asks sqlPool
  liftIO . Hasql.Pool.use pool $
    Transaction.transaction Serializable Write (Session.updateFilmRoll newSettings)
  logDebug "Wrote settings"
  env <- ask
  missing <- Preview.findMissingPreviews False
  void . liftIO $ MVar.tryPutMVar env.previewMVar missing
  logDebug $ "Updating " <> tshow (length missing) <> " preview(s)"
  pure newSettings

-- CHECK EXPRESSIONS

handleCheckExpressions :: [Expression] -> PositiveT Handler [ExpressionResult]
handleCheckExpressions exprs =
  let eval v expr =
        SampleEval $
          filter
            (not . isInfinite)
            [Language.eval p v expr - p | p <- [0.1, 0.2 .. 1.0]]
      parseAndCheck e =
        first TypeError . Language.check =<< first SyntaxError (Language.parse e.eExpr)
   in pure [either id (eval e.eValue) (parseAndCheck e) | e <- exprs]

-- GENERATE

handleGenerateHighRes :: Text -> ImageSettings -> PositiveT Handler NoContent
handleGenerateHighRes dir settings = do
  let input = Text.unpack dir </> Filename.toFilePath settings.iFilename
  env <- ask
  SingleImage.generate (Log.log env.logger) "Generating highres version: " input settings
  pure NoContent

handleGenerateWallpaper :: Text -> ImageSettings -> PositiveT Handler NoContent
handleGenerateWallpaper dir settings = do
  let input = Text.unpack dir </> Filename.toFilePath settings.iFilename
      outputBase homeDir =
        homeDir
          </> "Documents/wallpapers/positive"
          </> filter (\c -> not (isPathSeparator c || c == '.')) (Text.unpack dir)
          <> " | "
          <> Filename.toFilePath settings.iFilename
  log $ "Generating wallpaper version of: " <> Text.pack input
  output <- Util.ensureUniqueFilename . outputBase =<< liftIO Directory.getHomeDirectory
  image <-
    handleLeft $
      Image.fromDiskPreProcess (Just 2560) settings.iCrop input
  HIP.writeImage output $ Image.applySettings settings image
  NoContent <$ log ("Wrote wallpaper version of: " <> Text.pack input)

-- OPEN EXTERNALEDITOR

handleOpenExternalEditor :: Text -> ImageSettings -> PositiveT Handler NoContent
handleOpenExternalEditor dir settings = do
  let input = Text.unpack dir </> Filename.toFilePath settings.iFilename
  log $ "Opening in external editor: " <> Text.pack input
  image <-
    handleLeft $
      Image.fromDiskPreProcess Nothing settings.iCrop input
  HIP.displayImageUsing HIP.defaultViewer False (Image.applySettings settings image)
  pure NoContent

-- HISTOGRAM

handleGetSettingsHistogram :: Text -> ImageSettings -> PositiveT Handler [Int]
handleGetSettingsHistogram dir settings =
  let toHistogram arr =
        Massiv.Mutable.createArrayST_ @Massiv.P @_ @Int
          (Massiv.Sz1 (1 + fromIntegral (maxBound :: Word8)))
          $ \marr ->
            Massiv.forM_ arr $
              \(HIP.PixelY p) -> Massiv.modify marr (pure . (+) 1) (fromIntegral (HIP.toWord8 p))
   in do
        (image, putMVarBack) <-
          getCachedImage settings.iCrop $
            Text.pack (Text.unpack dir </> Filename.toFilePath settings.iFilename)
        liftIO putMVarBack
        logDebug $ "Creating histogram for: " <> Filename.toText settings.iFilename
        pure . Massiv.toList . toHistogram . HIP.unImage $
          Image.applySettings settings image

-- COORDINATE

handleGetCoordinateInfo ::
  Text ->
  ([(Double, Double)], ImageSettings) ->
  PositiveT Handler [CoordinateInfo]
handleGetCoordinateInfo dir (coordinates, settings) =
  let toInfo image (x, y) =
        CoordinateInfo x y
          . (\(HIP.PixelY p) -> HIP.toDouble p)
          . HIP.borderIndex Massiv.Continue image
          $ HIP.Ix2
            (floor (int2Double (HIP.rows image) * y))
            (floor (int2Double (HIP.cols image) * x))
   in do
        (image, putMVarBack) <-
          first (Image.applySettings settings)
            <$> getCachedImage settings.iCrop
              (Text.pack (Text.unpack dir </> Filename.toFilePath settings.iFilename))
        liftIO putMVarBack
        pure $ fmap (toInfo image) coordinates

-- LIST DIRECTORIES

handleGetSettings :: PositiveT Handler [(Text, FilmRoll)]
handleGetSettings = do
  pool <- asks sqlPool
  result <- liftIO $ Hasql.Pool.use pool Session.selectFilmRolls
  case result of
    Left _ -> throwError err500
    Right filmRolls -> pure $ HashMap.toList filmRolls

-- HANDLER HELPERS

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
  when env.isDev $
    liftIO . env.logger $ Log.format "debug" msg

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
