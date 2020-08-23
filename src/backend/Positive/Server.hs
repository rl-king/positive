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
import GHC.Float (int2Double)
import qualified Graphics.Image as HIP
import Network.Wai.Handler.Warp
import Positive.Flags
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
        :> Get '[JSON] ([ImageSettings], WorkingDirectory),
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
            (logMsg_ logger ("listening on port " <> tshow @Int 8080))
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
      logMsg "Apply and encode"
      let encoded = HIP.encode HIP.PNG [] . HIP.exchange HIP.VS $ processImage settings image
      logMsg "Apply and encode done"
      liftIO putMVarBack
      pure encoded

handleSaveSettings :: [ImageSettings] -> PositiveM [ImageSettings]
handleSaveSettings settings = do
  Env {ePreviewMVar} <- ask
  dir <- workingDirectory
  let newSettings = Settings.fromList settings
      path = dir </> "image-settings.json"
  liftIO . ByteString.writeFile path $ Aeson.encode newSettings
  logMsgDebug "Wrote settings"
  liftIO $ pSwapMVar ePreviewMVar newSettings
  pure $ Settings.toList newSettings

handleGetSettings :: PositiveM ([ImageSettings], WorkingDirectory)
handleGetSettings =
  (\a b -> (Settings.toList a, eDir b))
    <$> getSettingsFile <*> ask

-- GENERATE PREVIEWS

handleGenerateHighRes :: ImageSettings -> PositiveM NoContent
handleGenerateHighRes settings = do
  dir <- workingDirectory
  liftIO $ createDirectoryIfMissing False (dir </> "highres")
  let input = dir </> Text.unpack (iFilename settings)
      output = dir </> "highres" </> Text.unpack (iFilename settings)
  logMsg $ "Generating highres version of: " <> Text.pack input
  image <-
    liftIO $
      HIP.encode HIP.PNG [] . HIP.exchange HIP.VS . processImage settings
        <$> readImageFromDisk input
  liftIO $ ByteString.writeFile output image
  logMsg $ "Wrote highres version of: " <> Text.pack input
  pure NoContent

-- HISTOGRAM

handleGetSettingsHistogram :: Int -> ImageSettings -> PositiveM [Int]
handleGetSettingsHistogram previewWidth settings = do
  dir <- workingDirectory
  (image, putMVarBack) <-
    getImage previewWidth $
      Text.pack (dir </> Text.unpack (iFilename settings))
  liftIO putMVarBack
  logMsgDebug $ "Creating histogram for: " <> iFilename settings
  pure . Vector.toList . HIP.hBins . head . HIP.getHistograms $
    processImage settings image

-- SETTINGS FILE

getSettingsFile :: PositiveM FilmRollSettings
getSettingsFile = do
  dir <- workingDirectory
  let path = dir </> "image-settings.json"
  either (\e -> logMsg (tshow e) >> throwError err404) pure
    =<< liftIO (Aeson.eitherDecodeFileStrict path)

getAllPngs :: MonadIO m => FilePath -> m [Text]
getAllPngs dir =
  fmap Text.pack . filter ((".png" ==) . Path.takeExtension)
    <$> liftIO (listDirectory dir)

-- IMAGE

getImage :: Int -> Text -> PositiveM (MonochromeImage HIP.VU, IO ())
getImage previewWidth path = do
  Env {eImageMVar} <- ask
  currentlyLoaded@(loadedPath, loadedImage) <- liftIO $ takeMVar eImageMVar
  logMsgDebug $ "MVar: " <> tshow currentlyLoaded
  if path == loadedPath && HIP.cols loadedImage == previewWidth
    then do
      logMsgDebug "From cache"
      pure (loadedImage, putMVar eImageMVar currentlyLoaded)
    else do
      logMsgDebug "From disk"
      image <- liftIO $ resizeImage previewWidth <$> readImageFromDisk (Text.unpack path)
      pure (image, putMVar eImageMVar (path, image))

-- IMAGE

type MonochromeImage arr =
  HIP.Image arr HIP.Y Double

type MonochromePixel =
  HIP.Pixel HIP.Y Double

readImageFromDisk :: String -> IO (MonochromeImage HIP.VU)
readImageFromDisk =
  HIP.readImageY HIP.VU

resizeImage :: Int -> MonochromeImage HIP.VU -> MonochromeImage HIP.VU
resizeImage targetWidth image =
  let mul = int2Double (HIP.rows image) / int2Double (HIP.cols image)
   in HIP.resize
        (HIP.Bicubic (-0.25))
        HIP.Edge
        (round (int2Double targetWidth * mul), targetWidth)
        image

processImage :: ImageSettings -> MonochromeImage HIP.VU -> MonochromeImage HIP.VU
processImage is image =
  let (h, w) = HIP.dims image
      cropOffset =
        ( floor $ int2Double h / 100 * icTop (iCrop is),
          floor $ int2Double w / 100 * icLeft (iCrop is)
        )
      cropWidth = floor $ int2Double (w - snd cropOffset) * (icWidth (iCrop is) / 100)
      cropHeight = floor $ int2Double cropWidth * mul
      mul = int2Double h / int2Double w
   in HIP.map
        ( zone 0.9 (iZone9 is)
            . zone 0.5 (iZone5 is)
            . zone 0.1 (iZone1 is)
            . gamma (iGamma is)
            . compress (iBlackpoint is) (iWhitepoint is)
            . invert
        )
        -- NOTE: 'normalize' is the only "automatic" correction and has a subtle effect
        -- at which it makes smaller images have more contrast due to different max and min
        -- values vs the original size
        . HIP.normalize
        . (if iRotate is == 0 then id else HIP.rotate (HIP.Bicubic (-0.25)) (HIP.Fill 0) (iRotate is))
        $ HIP.crop cropOffset (cropHeight, cropWidth) image

-- FILTERS

invert :: MonochromePixel -> MonochromePixel
invert =
  fmap (1 -)
{-# INLINE invert #-}

compress :: Double -> Double -> MonochromePixel -> MonochromePixel
compress s l =
  fmap (\p -> min 1 . max 0 $ (p - s) / (l - s))
{-# INLINE compress #-}

gamma :: Double -> MonochromePixel -> MonochromePixel
gamma x =
  fmap (** x)
{-# INLINE gamma #-}

zone :: Double -> Double -> MonochromePixel -> MonochromePixel
zone t i =
  let m v = (1 - abs (v - t)) ^ (2 :: Int)
   in fmap (\v -> v + (i * m v))
{-# INLINE zone #-}

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
          logMsg_ logger "No preview settings file found, creating one now"
          filenames <- getAllPngs (Text.unpack dir)
          let settings = Settings.fromFilenames filenames
          liftIO $ createDirectoryIfMissing False (Text.unpack dir </> "previews")
          liftIO . ByteString.writeFile path $ Aeson.encode settings
          logMsg_ logger $ "Wrote: " <> Text.pack path
          pure $ Right settings
    case diffed of
      Left err -> logMsg_ logger $ Text.pack err
      Right newSettings -> do
        ByteString.writeFile path $ Aeson.encode queuedSettings
        generatePreviews logger newSettings wd
        threadDelay 10000000

generatePreviews :: TimedFastLogger -> FilmRollSettings -> WorkingDirectory -> IO ()
generatePreviews logger filmRoll (WorkingDirectory dir) =
  for_ (toList filmRoll) $
    \settings -> do
      let input = Text.unpack dir </> Text.unpack (iFilename settings)
          output = Text.unpack dir </> "previews" </> Path.replaceExtension (Text.unpack (iFilename settings)) ".jpg"
      logMsg_ logger $ "Generating preview for: " <> tshow input
      ByteString.writeFile output
        =<< HIP.encode HIP.JPG [] . HIP.exchange HIP.VS . processImage settings . resizeImage 750
        <$> readImageFromDisk input

-- LOG

logMsg :: Text -> PositiveM ()
logMsg msg = do
  Env {eLogger} <- ask
  liftIO . eLogger $ formatLog "info" msg

logMsgDebug :: Text -> PositiveM ()
logMsgDebug msg = do
  Env {eIsDev, eLogger} <- ask
  when eIsDev . liftIO . eLogger $ formatLog "debug" msg

logMsg_ :: TimedFastLogger -> Text -> IO ()
logMsg_ logger msg =
  logger (formatLog "info" msg)

formatLog :: Text -> Text -> FormattedTime -> LogStr
formatLog lvl msg time =
  toLogStr time <> " [" <> toLogStr lvl <> "] " <> toLogStr msg <> "\n"

-- PROFILE

timed :: Text -> a -> PositiveM a
timed name action = do
  logMsgDebug $ name <> " - started"
  start <- liftIO Time.getCurrentTime
  a <- liftIO $ evaluate action
  done <- liftIO Time.getCurrentTime
  logMsgDebug $ name <> " - processed in: " <> tshow (Time.diffUTCTime done start)
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
