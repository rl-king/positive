{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Positive.Server where

import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified Data.Vector.Unboxed as Vector
import qualified Graphics.Image as HIP
import Network.Wai.Handler.Warp
import Positive.Flags
import Positive.Image
import Positive.Prelude hiding (ByteString)
import Positive.Settings as Settings
import qualified Positive.Static as Static
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import System.Directory
import System.FilePath.Posix ((</>))
import qualified System.FilePath.Posix as Path
import System.Log.FastLogger (FormattedTime, LogStr, TimedFastLogger, toLogStr)

-- POSITIVE

type PositiveM =
  ReaderT Env Handler

data Env = Env
  { eImageMVar :: !(MVar (Text, MonochromeImage HIP.VU)),
    ePreviewMVar :: !(MVar FilmRollSettings),
    eDir :: !WorkingDirectory,
    eIsDev :: !Bool,
    eLogger :: !TimedFastLogger
  }

-- API

data Api route = Api
  { aSettingsApi :: route :- ToServantApi SettingsApi,
    aImageApi :: route :- ToServantApi ImageApi
  }
  deriving (Generic)

data ImageApi route = ImageApi
  { iaImage ::
      route :- "image"
        :> QueryParam' '[Required, Strict] "preview-width" Int
        :> QueryParam' '[Required, Strict] "image-settings" ImageSettings
        :> Get '[Image] ByteString,
    iaRaw :: route :- Raw
  }
  deriving (Generic)

data SettingsApi route = SettingsApi
  { saSaveSettings ::
      route :- "image" :> "settings"
        :> ReqBody '[JSON] [ImageSettings]
        :> Post '[JSON] [ImageSettings],
    saGetSettings ::
      route :- "image" :> "settings"
        :> Get '[JSON] ([ImageSettings], (WorkingDirectory, Text)),
    saGetSettingsHistogram ::
      route :- "image" :> "settings" :> "histogram"
        :> QueryParam' '[Required, Strict] "preview-width" Int
        :> ReqBody '[JSON] ImageSettings
        :> Post '[JSON] [Int],
    saGenerateHighRes ::
      route :- "image" :> "settings" :> "highres"
        :> ReqBody '[JSON] ImageSettings
        :> PostNoContent '[JSON] NoContent
  }
  deriving (Generic)

-- SERVER

server :: TimedFastLogger -> Flags -> IO ()
server logger Flags {fDir, fIsDev} =
  let settings =
        setPort 8080 $
          setBeforeMainLoop
            (log_ logger ("listening on port " <> tshow @Int 8080))
            defaultSettings
      nt imageMVar previewMVar =
        flip runReaderT (Env imageMVar previewMVar fDir fIsDev logger)
   in do
        imageMVar <- newMVar ("", HIP.fromLists [[HIP.PixelY 1]])
        previewMVar <- newEmptyMVar
        previewWorker logger previewMVar fDir
        runSettings settings (genericServeT (nt imageMVar previewMVar) (handlers fIsDev))

-- HANDLERS

handlers :: Bool -> Api (AsServerT PositiveM)
handlers isDev =
  Api
    { aImageApi =
        genericServerT
          ImageApi
            { iaImage = handleImage,
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

handleImage :: Int -> ImageSettings -> PositiveM ByteString
handleImage previewWidth settings = do
  dir <- workingDirectory
  Env {eIsDev} <- ask
  (image, putMVarBack) <-
    getImage previewWidth $
      Text.pack (dir </> Text.unpack (iFilename settings))
  if eIsDev
    then do
      processed <- timed "Apply" $ processImage settings image
      encoded <- timed "Encode" $ HIP.encode HIP.PNG [] $ HIP.exchange HIP.VS processed
      liftIO putMVarBack
      pure encoded
    else do
      log "Apply and encode"
      let encoded = HIP.encode HIP.PNG [] . HIP.exchange HIP.VS $ processImage settings image
      log "Apply and encode done"
      liftIO putMVarBack
      pure encoded

handleSaveSettings :: [ImageSettings] -> PositiveM [ImageSettings]
handleSaveSettings settings = do
  Env {ePreviewMVar} <- ask
  dir <- workingDirectory
  let newSettings = Settings.fromList settings
      path = dir </> "image-settings.json"
  liftIO . ByteString.writeFile path $ Aeson.encode newSettings
  logDebug "Wrote settings"
  liftIO $ pSwapMVar ePreviewMVar newSettings
  pure $ Settings.toList newSettings

handleGetSettings :: PositiveM ([ImageSettings], (WorkingDirectory, Text))
handleGetSettings = do
  dir <- asks eDir
  path <- liftIO . makeAbsolute =<< workingDirectory
  settings <- getSettingsFile
  pure (Settings.toList settings, (dir, Text.pack path))

-- GENERATE PREVIEWS

handleGenerateHighRes :: ImageSettings -> PositiveM NoContent
handleGenerateHighRes settings = do
  dir <- workingDirectory
  liftIO $ createDirectoryIfMissing False (dir </> "highres")
  let input = dir </> Text.unpack (iFilename settings)
      output = dir </> "highres" </> Text.unpack (iFilename settings)
  log $ "Generating highres version of: " <> Text.pack input
  image <-
    liftIO $
      HIP.encode HIP.PNG [] . HIP.exchange HIP.VS . processImage settings
        <$> readImageFromDisk input
  liftIO $ ByteString.writeFile output image
  log $ "Wrote highres version of: " <> Text.pack input
  pure NoContent

-- HISTOGRAM

handleGetSettingsHistogram :: Int -> ImageSettings -> PositiveM [Int]
handleGetSettingsHistogram previewWidth settings = do
  dir <- workingDirectory
  (image, putMVarBack) <-
    getImage previewWidth $
      Text.pack (dir </> Text.unpack (iFilename settings))
  liftIO putMVarBack
  logDebug $ "Creating histogram for: " <> iFilename settings
  pure . Vector.toList . HIP.hBins . head . HIP.getHistograms $
    processImage settings image

-- SETTINGS FILE

getSettingsFile :: PositiveM FilmRollSettings
getSettingsFile = do
  dir <- workingDirectory
  let path = dir </> "image-settings.json"
  either (\e -> log (tshow e) >> throwError err404) pure
    =<< liftIO (Aeson.eitherDecodeFileStrict path)

-- IMAGE

getImage :: Int -> Text -> PositiveM (MonochromeImage HIP.VU, IO ())
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
      image <- liftIO $ resizeImage previewWidth <$> readImageFromDisk (Text.unpack path)
      pure (image, putMVar eImageMVar (path, image))

-- PREVIEW LOOP

previewWorker :: TimedFastLogger -> MVar FilmRollSettings -> WorkingDirectory -> IO ()
previewWorker logger previewMVar wd@(WorkingDirectory dir) =
  void . forkIO . forever $ do
    let path = Text.unpack dir </> "previews" </> "image-settings.json"
    queuedSettings <- takeMVar previewMVar
    diffed <- do
      exists <- doesPathExist path
      if exists
        then fmap (Settings.difference queuedSettings) <$> Aeson.eitherDecodeFileStrict path
        else do
          log_ logger "No preview settings file found, creating one now"
          liftIO $ createDirectoryIfMissing False (Text.unpack dir </> "previews")
          liftIO . ByteString.writeFile path $ Aeson.encode queuedSettings
          log_ logger $ "Wrote: " <> Text.pack path
          pure $ Right queuedSettings
    case diffed of
      Left err -> log_ logger $ Text.pack err
      Right newSettings -> do
        ByteString.writeFile path $ Aeson.encode queuedSettings
        generatePreviews logger newSettings wd
        threadDelay 30000000

generatePreviews :: TimedFastLogger -> FilmRollSettings -> WorkingDirectory -> IO ()
generatePreviews logger filmRoll (WorkingDirectory dir) =
  for_ (sortOn iFilename (toList filmRoll)) $
    \settings -> do
      let input = Text.unpack dir </> Text.unpack (iFilename settings)
          output = Text.unpack dir </> "previews" </> Path.replaceExtension (Text.unpack (iFilename settings)) ".jpg"
      log_ logger $ "Generating preview for: " <> tshow input
      ByteString.writeFile output
        =<< HIP.encode HIP.JPG [] . HIP.exchange HIP.VS . processImage settings . resizeImage 750
        <$> readImageFromDisk input

-- LOG

log :: Text -> PositiveM ()
log msg = do
  Env {eLogger} <- ask
  liftIO . eLogger $ formatLog "info" msg

logDebug :: Text -> PositiveM ()
logDebug msg = do
  Env {eIsDev, eLogger} <- ask
  when eIsDev . liftIO . eLogger $ formatLog "debug" msg

log_ :: TimedFastLogger -> Text -> IO ()
log_ logger msg =
  logger (formatLog "info" msg)

formatLog :: Text -> Text -> FormattedTime -> LogStr
formatLog lvl msg time =
  toLogStr time <> " [" <> toLogStr lvl <> "] " <> toLogStr msg <> "\n"

-- PROFILE

timed :: Text -> a -> PositiveM a
timed name action = do
  logDebug $ name <> " - started"
  start <- liftIO Time.getCurrentTime
  a <- liftIO $ evaluate action
  done <- liftIO Time.getCurrentTime
  logDebug $ name <> " - processed in: " <> tshow (Time.diffUTCTime done start)
  pure a

-- HELPERS

workingDirectory :: PositiveM FilePath
workingDirectory =
  asks (toFilePath . eDir)

pSwapMVar :: MVar a -> a -> IO ()
pSwapMVar mvar a = do
  isEmpty <- isEmptyMVar mvar
  if isEmpty
    then putMVar mvar a
    else void $ swapMVar mvar a
