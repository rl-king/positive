{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Positive.Server
  ( run,
  )
where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Massiv.Array.IO as Massiv
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified Graphics.Image as HIP
import Network.Wai.EventSource
import Network.Wai.Handler.Warp hiding (run)
import Positive.Api
import Positive.Flags
import Positive.Image
import qualified Positive.Log as Log
import Positive.Prelude hiding (ByteString)
import Positive.Settings as Settings
import qualified Positive.Static as Static
import Servant
import Servant.Server.Generic
import System.Directory
import System.FilePath.Posix ((</>))

-- POSITIVE

type PositiveT m =
  ReaderT Env m

data Env = Env
  { eImageMVar :: !(MVar (Text, MonochromeImage)),
    eEventChan :: !(Chan ServerEvent),
    eIsDev :: !Bool,
    eLogger :: !Log.TimedFastLogger
  }

-- SERVER

run :: Log.TimedFastLogger -> Flags -> IO ()
run logger Flags {fIsDev} =
  let settings =
        setPort 8080 $
          setBeforeMainLoop
            (Log.log logger ("listening on port " <> tshow @Int 8080))
            defaultSettings
   in do
        imageMVar <- newMVar ("", HIP.fromLists [[HIP.PixelY 1]])
        eventChan <- newChan
        let env = Env imageMVar eventChan fIsDev logger
        runSettings settings (genericServeT (`runReaderT` env) (handlers fIsDev eventChan))

-- HANDLERS

handlers :: Bool -> Chan ServerEvent -> Api (AsServerT (PositiveT Handler))
handlers isDev chan =
  Api
    { aImageApi =
        genericServerT
          ImageApi
            { iaImage = handleImage,
              iaEvents = pure $ eventSourceAppChan chan,
              iaRaw = Static.serve isDev
            },
      aSettingsApi =
        genericServerT
          SettingsApi
            { saSaveSettings = handleSaveSettings,
              saGetSettings = handleGetSettings,
              saGetSettingsHistogram = handleGetSettingsHistogram,
              saGenerateHighRes = handleGenerateHighRes
            }
    }

handleImage :: Text -> Int -> ImageSettings -> PositiveT Handler ByteString
handleImage dir previewWidth settings = do
  Env {eIsDev} <- ask
  (image, putMVarBack) <-
    getImage previewWidth $
      Text.pack (Text.unpack dir </> Text.unpack (iFilename settings))
  if eIsDev
    then do
      processed <- timed "Apply" $ processImage settings image
      encoded <- timed "Encode" =<< encode "_.png" processed
      liftIO putMVarBack
      pure encoded
    else do
      log $ "Apply settings and encode: " <> iFilename settings
      encoded <- encode "_.png" $ processImage settings image
      liftIO putMVarBack
      pure encoded

encode ::
  ( MonadIO m,
    Massiv.ColorSpace (HIP.DefSpace cs) i e,
    Massiv.ColorSpace (Massiv.BaseSpace (HIP.DefSpace cs)) i e
  ) =>
  FilePath ->
  HIP.Image cs e ->
  m ByteString
encode path image =
  liftIO . Massiv.encodeImageM Massiv.imageWriteAutoFormats path $
    HIP.unImage (HIP.toDefSpace image)

handleSaveSettings :: Text -> [ImageSettings] -> PositiveT Handler [ImageSettings]
handleSaveSettings dir settings = do
  log dir
  let newSettings = Settings.fromList settings
      path = Text.unpack dir </> "image-settings.json"
  liftIO $ Aeson.encodeFile path newSettings
  logDebug "Wrote settings"
  pure $ Settings.toList newSettings

-- GENERATE PREVIEWS

handleGenerateHighRes :: Text -> ImageSettings -> PositiveT Handler NoContent
handleGenerateHighRes dir settings = do
  liftIO $ createDirectoryIfMissing False (Text.unpack dir </> "highres")
  let input = Text.unpack dir </> Text.unpack (iFilename settings)
      output = Text.unpack dir </> "highres" </> Text.unpack (iFilename settings)
  log $ "Generating highres version of: " <> Text.pack input
  maybeImage <- liftIO $ readImageFromDisk input
  case maybeImage of
    Left _ ->
      log "Image read error" >> throwError err404
    Right image -> do
      liftIO . HIP.writeImage output $ processImage settings image
      log $ "Wrote highres version of: " <> Text.pack input
      pure NoContent

-- HISTOGRAM

handleGetSettingsHistogram :: Text -> Int -> ImageSettings -> PositiveT Handler [Int]
handleGetSettingsHistogram dir previewWidth settings = do
  pure []

-- (image, putMVarBack) <-
--   getImage previewWidth $
--     Text.pack (Text.unpack dir </> Text.unpack (iFilename settings))
-- liftIO putMVarBack
-- logDebug $ "Creating histogram for: " <> iFilename settings
-- -- pure . Vector.toList . HIP.hBins . head . HIP.getHistograms $
--   processImage settings image
-- pure []

-- LIST DIRECTORIES

handleGetSettings :: PositiveT Handler [(Text, [ImageSettings])]
handleGetSettings =
  HashMap.toList . fmap toList <$> liftIO findImageSettings

-- SETTINGS FILE

getSettingsFile :: Text -> PositiveT Handler FilmRollSettings
getSettingsFile dir = do
  let path = Text.unpack dir </> "image-settings.json"
  either (\e -> log (tshow e) >> throwError err404) pure
    =<< liftIO (Aeson.eitherDecodeFileStrict path)

-- IMAGE

getImage :: Int -> Text -> PositiveT Handler (MonochromeImage, IO ())
getImage previewWidth path = do
  Env {eImageMVar} <- ask
  currentlyLoaded@(loadedPath, loadedImage) <- liftIO $ takeMVar eImageMVar
  logDebug $ "MVar: " <> tshow currentlyLoaded
  if path == loadedPath && HIP.cols loadedImage == previewWidth
    then do
      logDebug "From cache"
      pure (loadedImage, putMVar eImageMVar currentlyLoaded)
    else do
      logDebug "From disk"
      maybeImage <- liftIO $ readImageFromDisk (Text.unpack path)
      case maybeImage of
        Left err -> log ("Image read error: " <> tshow err) >> throwError err404
        Right image ->
          let resized = resizeImage previewWidth image
           in pure (resized, putMVar eImageMVar (path, resized))

-- LOG

log :: MonadIO m => Text -> PositiveT m ()
log msg = do
  Env {eLogger, eEventChan} <- ask
  liftIO . writeChan eEventChan $ ServerEvent (Just "log") Nothing [Builder.byteString $ encodeUtf8 msg]
  liftIO . eLogger $ Log.format "info" msg

logDebug :: MonadIO m => Text -> PositiveT m ()
logDebug msg = do
  Env {eIsDev, eLogger, eEventChan} <- ask
  when eIsDev $ do
    liftIO . writeChan eEventChan $ ServerEvent (Just "log") Nothing [Builder.byteString $ encodeUtf8 msg]
    liftIO . eLogger $ Log.format "debug" msg

-- PROFILE

timed :: Text -> a -> PositiveT Handler a
timed name action = do
  logDebug $ name <> " - started"
  start <- liftIO Time.getCurrentTime
  a <- liftIO $ evaluate action
  done <- liftIO Time.getCurrentTime
  logDebug $ name <> " - processed in: " <> tshow (Time.diffUTCTime done start)
  pure a

-- HELPERS

pSwapMVar :: MVar a -> a -> IO ()
pSwapMVar mvar a = do
  success <- tryPutMVar mvar a
  unless success (void $ swapMVar mvar a)
