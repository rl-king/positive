{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Positive.Server where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Massiv.Array.IO as Massiv
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified Graphics.Image as HIP
import Network.Wai.EventSource
import Network.Wai.Handler.Warp
import Positive.Api
import Positive.Flags
import Positive.Image
import Positive.Prelude hiding (ByteString)
import Positive.Settings as Settings
import qualified Positive.Static as Static
import Servant
import Servant.Server.Generic
import System.Directory
import System.FilePath.Posix ((</>))
import qualified System.FilePath.Posix as Path
import System.Log.FastLogger (FormattedTime, LogStr, TimedFastLogger, toLogStr)

-- POSITIVE

type PositiveT m =
  ReaderT Env m

data Env = Env
  { eImageMVar :: !(MVar (Text, MonochromeImage)),
    ePreviewMVar :: !(MVar FilmRollSettings),
    eEventChan :: !(Chan ServerEvent),
    eDir :: !WorkingDirectory,
    eIsDev :: !Bool,
    eLogger :: !TimedFastLogger
  }

-- SERVER

server :: TimedFastLogger -> Flags -> IO ()
server logger Flags {fDir, fIsDev} =
  let settings =
        setPort 8080 $
          setBeforeMainLoop
            (log_ logger ("listening on port " <> tshow @Int 8080))
            defaultSettings
   in do
        imageMVar <- newMVar ("", HIP.fromLists [[HIP.PixelY 1]])
        previewMVar <- newEmptyMVar
        eventChan <- newChan
        let env = Env imageMVar previewMVar eventChan fDir fIsDev logger
        void . forkIO . forever $ runReaderT previewWorker env
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
              saGenerateHighRes = handleGenerateHighRes,
              saListDirectories = handleListDirectories
            }
    }

handleImage :: Int -> ImageSettings -> PositiveT Handler ByteString
handleImage previewWidth settings = do
  dir <- workingDirectory
  Env {eIsDev} <- ask
  (image, putMVarBack) <-
    getImage previewWidth $
      Text.pack (dir </> Text.unpack (iFilename settings))
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

handleSaveSettings :: [ImageSettings] -> PositiveT Handler [ImageSettings]
handleSaveSettings settings = do
  Env {ePreviewMVar} <- ask
  dir <- workingDirectory
  let newSettings = Settings.fromList settings
      path = dir </> "image-settings.json"
  liftIO . ByteString.writeFile path $ Aeson.encode newSettings
  logDebug "Wrote settings"
  liftIO $ pSwapMVar ePreviewMVar =<< diffedPreviewSettings newSettings dir
  pure $ Settings.toList newSettings

handleGetSettings :: PositiveT Handler ([ImageSettings], (WorkingDirectory, Text))
handleGetSettings = do
  dir <- asks eDir
  path <- liftIO . makeAbsolute =<< workingDirectory
  settings <- getSettingsFile
  pure (Settings.toList settings, (dir, Text.pack path))

-- GENERATE PREVIEWS

handleGenerateHighRes :: ImageSettings -> PositiveT Handler NoContent
handleGenerateHighRes settings = do
  dir <- workingDirectory
  liftIO $ createDirectoryIfMissing False (dir </> "highres")
  let input = dir </> Text.unpack (iFilename settings)
      output = dir </> "highres" </> Text.unpack (iFilename settings)
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

handleGetSettingsHistogram :: Int -> ImageSettings -> PositiveT Handler [Int]
handleGetSettingsHistogram previewWidth settings = do
  dir <- workingDirectory
  (image, putMVarBack) <-
    getImage previewWidth $
      Text.pack (dir </> Text.unpack (iFilename settings))
  liftIO putMVarBack
  logDebug $ "Creating histogram for: " <> iFilename settings
  -- pure . Vector.toList . HIP.hBins . head . HIP.getHistograms $
  --   processImage settings image
  pure []

-- LIST DIRECTORIES

handleListDirectories :: PositiveT Handler Fs
handleListDirectories = do
  dir <- workingDirectory
  liftIO $ Settings.listPreviews dir

-- SETTINGS FILE

getSettingsFile :: PositiveT Handler FilmRollSettings
getSettingsFile = do
  dir <- workingDirectory
  let path = dir </> "image-settings.json"
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
        Left _ -> log "Image read error" >> throwError err404
        Right image ->
          let resized = resizeImage previewWidth image
           in pure (resized, putMVar eImageMVar (path, resized))

-- PREVIEW LOOP

previewWorker :: PositiveT IO ()
previewWorker = do
  Env {eImageMVar, ePreviewMVar, eEventChan} <- ask
  dir <- workingDirectory
  let path = dir </> "previews" </> "image-settings.json"
  queuedSettings <- liftIO $ takeMVar ePreviewMVar
  case grab queuedSettings of
    Nothing -> pure ()
    Just (settings, rest) -> do
      -- Block till image is done processing
      _ <- liftIO $ readMVar eImageMVar
      log $ "Generating preview for: " <> iFilename settings
      liftIO $ insertPreviewSettings path settings
      let input = dir </> Text.unpack (iFilename settings)
          output = dir </> "previews" </> Path.replaceExtension (Text.unpack (iFilename settings)) ".jpg"
      maybeImage <- liftIO $ readImageFromDisk input
      case maybeImage of
        Left _ -> do
          log $
            Text.unwords
              ["Error generating preview for:", iFilename settings, "/", tshow (length (Settings.toList rest)), "more in queue"]
          liftIO . void $ tryPutMVar ePreviewMVar rest
        Right image -> do
          liftIO . HIP.writeImage output $ processImage settings (resizeImage 750 image)
          log $
            Text.unwords
              ["Generated preview for:", iFilename settings, "/", tshow (length (Settings.toList rest)), "more in queue"]
          liftIO $ writeChan eEventChan (ServerEvent (Just "preview") Nothing [Builder.byteString . encodeUtf8 $ iFilename settings])
          liftIO . void $ tryPutMVar ePreviewMVar rest

diffedPreviewSettings :: FilmRollSettings -> FilePath -> IO FilmRollSettings
diffedPreviewSettings filmRoll dir = do
  let path = dir </> "previews" </> "image-settings.json"
  exists <- doesPathExist path
  unless exists . ByteString.writeFile path . Aeson.encode $ fromList []
  Just current <- Aeson.decodeFileStrict path
  pure $ Settings.difference filmRoll current

insertPreviewSettings :: FilePath -> ImageSettings -> IO ()
insertPreviewSettings path settings = do
  Just filmRoll <- Aeson.decodeFileStrict path
  ByteString.writeFile path . Aeson.encode $ Settings.insert settings filmRoll

-- LOG

log :: MonadIO m => Text -> PositiveT m ()
log msg = do
  Env {eLogger, eEventChan} <- ask
  liftIO . writeChan eEventChan $ ServerEvent (Just "log") Nothing [Builder.byteString $ encodeUtf8 msg]
  liftIO . eLogger $ formatLog "info" msg

logDebug :: MonadIO m => Text -> PositiveT m ()
logDebug msg = do
  Env {eIsDev, eLogger, eEventChan} <- ask
  when eIsDev $ do
    liftIO . writeChan eEventChan $ ServerEvent (Just "log") Nothing [Builder.byteString $ encodeUtf8 msg]
    liftIO . eLogger $ formatLog "debug" msg

log_ :: TimedFastLogger -> Text -> IO ()
log_ logger msg =
  logger (formatLog "info" msg)

formatLog :: Text -> Text -> FormattedTime -> LogStr
formatLog lvl msg time =
  toLogStr time <> " [" <> toLogStr lvl <> "] " <> toLogStr msg <> "\n"

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

workingDirectory :: Monad m => PositiveT m FilePath
workingDirectory =
  asks (toFilePath . eDir)

pSwapMVar :: MVar a -> a -> IO ()
pSwapMVar mvar a = do
  success <- tryPutMVar mvar a
  unless success (void $ swapMVar mvar a)
