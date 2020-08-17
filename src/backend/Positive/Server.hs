{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Positive.Server where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified Data.Vector.Unboxed as Vector
import GHC.Float (int2Double)
import qualified Graphics.Image as HIP
import qualified Network.HTTP.Media as Media
import Network.Wai.Handler.Warp
import Positive.Settings as Settings
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import System.Directory
import System.FilePath.Posix ((</>))
import qualified System.FilePath.Posix as Path
import System.Log.FastLogger (TimedFastLogger, toLogStr)

-- POSITIVE

type PositiveM =
  ReaderT Env Handler

data Env = Env
  { eImageMVar :: MVar (Text, MonochromeImage HIP.VU),
    eDir :: Dir,
    eLogger :: TimedFastLogger
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
        :> QueryParam' '[Required, Strict] "dir" Text
        :> QueryParam' '[Required, Strict] "preview-width" Int
        :> QueryParam' '[Required, Strict] "image-settings" ImageSettings
        :> Get '[Image] ByteString,
    iaRaw :: route :- Raw
  }
  deriving (Generic)

data SettingsApi route = SettingsApi
  { saSaveSettings ::
      route :- "image"
        :> "settings"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] [ImageSettings]
        :> Post '[JSON] [ImageSettings],
    saGetSettings ::
      route :- "image"
        :> "settings"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> Get '[JSON] [ImageSettings],
    saGetSettingsHistogram ::
      route :- "image"
        :> "settings"
        :> "histogram"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> QueryParam' '[Required, Strict] "preview-width" Int
        :> ReqBody '[JSON] ImageSettings
        :> Post '[JSON] [Int],
    saGenerateHighRes ::
      route :- "image"
        :> "settings"
        :> "highres"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] ImageSettings
        :> PostNoContent '[JSON] NoContent,
    saGeneratePreviews ::
      route :- "image"
        :> "settings"
        :> "previews"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> PostNoContent '[JSON] NoContent
  }
  deriving (Generic)

-- SERVER

server :: TimedFastLogger -> Dir -> IO ()
server logger dir =
  let settings =
        setPort 8080 $
          setBeforeMainLoop
            (logMsg_ logger ("listening on port " <> tshow @Int 8080))
            defaultSettings
      nt ref =
        flip runReaderT (Env ref dir logger)
   in do
        ref <- newMVar ("", HIP.fromLists [[HIP.PixelY 1]])
        runSettings settings (genericServeT (nt ref) handlers)

-- HANDLERS

handlers :: Api (AsServerT PositiveM)
handlers =
  Api
    { aImageApi =
        genericServerT
          ImageApi
            { iaImage = handleImage,
              iaRaw = serveDirectoryFileServer "./"
            },
      aSettingsApi =
        genericServerT
          SettingsApi
            { saSaveSettings = handleSaveSettings,
              saGetSettings = handleGetSettings,
              saGetSettingsHistogram = handleGetSettingsHistogram,
              saGenerateHighRes = handleGenerateHighRes,
              saGeneratePreviews = handleGeneratePreviews
            }
    }

handleImage :: Text -> Int -> ImageSettings -> PositiveM ByteString
handleImage dir previewWidth imageSettings = do
  (image, onDone) <-
    getImage previewWidth $
      Text.pack (Text.unpack dir </> Text.unpack (iFilename imageSettings))
  processed <- timed "Apply settings" $ processImage imageSettings image
  encoded <- timed "Encode JPG" $ HIP.encode HIP.JPG [] $ HIP.exchange HIP.VS processed
  liftIO onDone
  pure encoded

handleSaveSettings :: Text -> [ImageSettings] -> PositiveM [ImageSettings]
handleSaveSettings dir imageSettings = do
  -- Maybe remove this, we could just write?
  settingsFile <- getSettingsFile dir
  case settingsFile of
    Left err -> do
      logMsg $ Text.pack err
      throwError err500
    Right _ -> do
      let newSettings = Settings.fromList imageSettings
          path = Text.unpack dir </> "image-settings.json"
      liftIO . ByteString.writeFile path $ Aeson.encode newSettings
      logMsg "Updated settings"
      pure $ Settings.toList newSettings

handleGetSettings :: Text -> PositiveM [ImageSettings]
handleGetSettings dir = do
  settingsFile <- getSettingsFile dir
  case settingsFile of
    Left err -> do
      logMsg $ Text.pack err
      throwError err500
    Right settings ->
      pure $ Settings.toList settings

-- GENERATE PREVIEWS

handleGeneratePreviews :: Text -> PositiveM NoContent
handleGeneratePreviews dir = do
  filmRollSettings <- getSettingsFile dir
  Env {eLogger} <- ask
  case filmRollSettings of
    Left _ -> pure NoContent
    Right (FilmRollSettings files) ->
      (NoContent <$) . liftIO . forkIO . for_ files $
        \settings -> do
          let input = Text.unpack dir </> Text.unpack (iFilename settings)
              output = Text.unpack dir </> "previews" </> Path.replaceExtension (Text.unpack (iFilename settings)) ".jpg"
          logMsg_ eLogger $ "Generating preview for: " <> Text.pack input
          ByteString.writeFile output
            =<< HIP.encode HIP.JPG [] . HIP.exchange HIP.VS . processImage settings . resizeImage 350
            <$> readImageFromDisk input

-- GENERATE PREVIEWS

handleGenerateHighRes :: Text -> ImageSettings -> PositiveM NoContent
handleGenerateHighRes dir settings =
  let input = Text.unpack dir </> Text.unpack (iFilename settings)
      output = Text.unpack dir </> "highres" </> Text.unpack (iFilename settings)
   in do
        logMsg $ "Generating highres version of: " <> Text.pack input
        image <-
          liftIO $
            HIP.encode HIP.PNG [] . HIP.exchange HIP.VS . processImage settings
              <$> readImageFromDisk input
        liftIO $ ByteString.writeFile output image
        pure NoContent

-- HISTOGRAM

handleGetSettingsHistogram :: Text -> Int -> ImageSettings -> PositiveM [Int]
handleGetSettingsHistogram dir previewWidth imageSettings = do
  (image, onDone) <-
    getImage previewWidth $
      Text.pack (Text.unpack dir </> Text.unpack (iFilename imageSettings))
  liftIO onDone
  pure . Vector.toList . HIP.hBins . head
    . HIP.getHistograms
    $ processImage imageSettings image

-- SETTINGS FILE

getSettingsFile :: Text -> PositiveM (Either String FilmRollSettings)
getSettingsFile dir = do
  let path = Text.unpack dir </> "image-settings.json"
  exists <- liftIO $ doesPathExist path
  if exists
    then liftIO $ Aeson.eitherDecodeFileStrict path
    else do
      logMsg "No settings file found, creating one now"
      filenames <- getAllPngs dir
      let settings = Settings.fromFilenames filenames
      liftIO . ByteString.writeFile path $ Aeson.encode settings
      logMsg $ "Wrote: " <> Text.pack path
      pure (Right settings)

getAllPngs :: MonadIO m => Text -> m [Text]
getAllPngs dir = do
  files <- liftIO $ listDirectory (Text.unpack dir)
  pure $ Text.pack <$> filter (\p -> Path.takeExtension p == ".png") files

-- IMAGE

getImage :: Int -> Text -> PositiveM (MonochromeImage HIP.VU, IO ())
getImage previewWidth path = do
  Env {eImageMVar} <- ask
  currentlyLoaded@(loadedPath, loadedImage) <- liftIO $ takeMVar eImageMVar
  logMsg $ "MVar: " <> tshow currentlyLoaded
  if path == loadedPath && HIP.cols loadedImage == previewWidth
    then do
      logMsg "Loaded image"
      pure (loadedImage, putMVar eImageMVar currentlyLoaded)
    else do
      logMsg "Reading image"
      image <- liftIO $ resizeImage previewWidth <$> readImageFromDisk (Text.unpack path)
      logMsg "Read image"
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
resizeImage previewWidth image =
  let mul = int2Double (HIP.rows image) / int2Double (HIP.cols image)
   in HIP.resize HIP.Bilinear HIP.Edge (round (int2Double previewWidth * mul), previewWidth) $
        HIP.resize HIP.Bilinear HIP.Edge (1800, 1200) image

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
        ( zone 0.95 (iZone9 is)
            . zone 0.5 (iZone5 is)
            . zone 0.15 (iZone1 is)
            . gamma (iGamma is)
            . compress (iBlackpoint is) (iWhitepoint is)
            . invert
        )
        . HIP.normalize
        . HIP.rotate HIP.Bilinear (HIP.Fill 0) (iRotate is)
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
  let m v = (1 - abs (v - t)) * (1 - abs (v - t))
   in fmap (\v -> v + (i * m v))
{-# INLINE zone #-}

-- IMAGE

data Image

instance Accept Image where
  contentType _ =
    "image" Media.// "jpg"

instance MimeRender Image ByteString where
  mimeRender _ = id

-- LOG

logMsg :: Text -> PositiveM ()
logMsg msg = do
  Env {eLogger} <- ask
  liftIO $ eLogger (\time -> toLogStr time <> " | " <> toLogStr msg <> "\n")

logMsg_ :: TimedFastLogger -> Text -> IO ()
logMsg_ logger msg =
  logger (\time -> toLogStr time <> " | " <> toLogStr msg <> "\n")

tshow :: Show a => a -> Text
tshow =
  Text.pack . show

-- PROFILE

timed :: Text -> a -> PositiveM a
timed name action = do
  logMsg $ name <> " - started"
  start <- liftIO Time.getCurrentTime
  a <- liftIO $ evaluate action
  done <- liftIO Time.getCurrentTime
  logMsg $ name <> " - processed in: " <> tshow (Time.diffUTCTime done start)
  pure a
