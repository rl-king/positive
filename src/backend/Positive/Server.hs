{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Positive.Server where

import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import GHC.Float (int2Float)
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

-- STATE

newtype LoadedImage
  = LoadedImage (MVar (Text, MonochromeImage HIP.VU))

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
        :> ReqBody '[JSON] ImageSettings
        :> PostNoContent '[JSON] NoContent,
    saGetSettings ::
      route :- "image"
        :> "settings"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> Get '[JSON] [ImageSettings],
    saListDirectory ::
      route :- "image"
        :> "list"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> Get '[JSON] [Text]
  }
  deriving (Generic)

-- SERVER

server :: TimedFastLogger -> IO ()
server logger =
  let settings =
        setPort 8080 $
          setBeforeMainLoop
            (logMsg logger ("listening on port " <> tshow @Int 8080))
            defaultSettings
   in do
        ref <- newEmptyMVar
        runSettings settings (genericServeT id (handlers logger (LoadedImage ref)))

-- HANDLERS

handlers :: TimedFastLogger -> LoadedImage -> Api (AsServerT Handler)
handlers logger state =
  Api
    { aImageApi =
        genericServerT
          ImageApi
            { iaImage = handleImage logger state,
              iaRaw = serveDirectoryFileServer "./"
            },
      aSettingsApi =
        genericServerT
          SettingsApi
            { saSaveSettings = handleSaveSettings logger,
              saGetSettings = handleGetSettings logger,
              saListDirectory = handleDirectory
            }
    }

handleImage :: TimedFastLogger -> LoadedImage -> Text -> Int -> ImageSettings -> Servant.Handler ByteString
handleImage logger state dir previewWidth imageSettings = do
  image <-
    getImage logger state previewWidth $
      Text.pack (Text.unpack dir </> Text.unpack (iFilename imageSettings))
  logMsg logger "Processing image"
  start <- liftIO Time.getCurrentTime
  processed <- liftIO . evaluate $ processImage imageSettings image
  processDone <- liftIO Time.getCurrentTime
  logMsg logger $ "processed in: " <> tshow (Time.diffUTCTime processDone start)
  startEncode <- liftIO Time.getCurrentTime
  image2 <- liftIO . evaluate . HIP.encode HIP.JPG [] $ HIP.exchange HIP.VS processed
  encodeDone <- liftIO Time.getCurrentTime
  logMsg logger $ "Encoded in: " <> tshow (Time.diffUTCTime encodeDone startEncode)
  pure image2

handleSaveSettings :: TimedFastLogger -> Text -> ImageSettings -> Servant.Handler NoContent
handleSaveSettings logger dir imageSettings = do
  let path = Text.unpack dir </> "image-settings.json"
  exists <- liftIO $ doesPathExist path
  if exists
    then do
      result :: Either String FilmRollSettings <- liftIO $ Aeson.eitherDecodeFileStrict path
      case result of
        Left err -> do
          logMsg logger $ Text.pack err
          throwError err500
        Right settings -> do
          liftIO . ByteString.writeFile path . Aeson.encode $ Settings.insert imageSettings settings
          logMsg logger "Updated settings"
          pure NoContent
    else do
      logMsg logger "No settings file found, creating one now"
      liftIO . ByteString.writeFile path . Aeson.encode $ Settings.init imageSettings
      logMsg logger $ "Wrote: " <> Text.pack path
      pure NoContent

handleGetSettings :: TimedFastLogger -> Text -> Servant.Handler [ImageSettings]
handleGetSettings logger dir = do
  let path = Text.unpack dir </> "image-settings.json"
  exists <- liftIO $ doesPathExist path
  if not exists
    then throwError err404
    else do
      result :: Either String FilmRollSettings <- liftIO $ Aeson.eitherDecodeFileStrict path
      case result of
        Left err -> do
          logMsg logger $ Text.pack err
          throwError err500
        Right settings ->
          pure $ Settings.toList settings

handleDirectory :: Text -> Servant.Handler [Text]
handleDirectory dir = do
  files <- liftIO $ listDirectory (Text.unpack dir)
  pure $ Text.pack <$> filter (\p -> Path.takeExtension p == ".png") files

getImage :: MonadIO m => TimedFastLogger -> LoadedImage -> Int -> Text -> m (MonochromeImage HIP.VU)
getImage logger state@(LoadedImage ref) previewWidth path = do
  maybeImage <- liftIO $ tryTakeMVar ref
  logMsg logger $ "MVar: " <> tshow maybeImage
  case maybeImage of
    Nothing -> do
      logMsg logger "Reading image"
      image <- liftIO $ resizeImage previewWidth <$> readImageFromDisk (Text.unpack path)
      logMsg logger "Read image"
      liftIO $ putMVar ref (path, image)
      logMsg logger "Updated MVar"
      pure image
    Just loadedImage@(cachedPath, cachedImage)
      | path == cachedPath && HIP.cols cachedImage == previewWidth -> do
        logMsg logger "From MVar image"
        liftIO $ putMVar ref loadedImage
        pure cachedImage
      | otherwise -> liftIO $ getImage logger state previewWidth path

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
  let mul = int2Float (HIP.rows image) / int2Float (HIP.cols image)
   in HIP.resize HIP.Bilinear HIP.Edge (round (int2Float previewWidth * mul), previewWidth) $
        HIP.resize HIP.Bilinear HIP.Edge (1800, 1200) image

processImage :: ImageSettings -> MonochromeImage HIP.VU -> MonochromeImage HIP.VU
processImage is =
  HIP.map $
    whitepoint (iWhitepoint is)
      . blackpoint (iBlackpoint is)
      . zone 0.95 (iZone9 is)
      . zone 0.5 (iZone5 is)
      . zone 0.15 (iZone1 is)
      . gamma (iGamma is)
      . invert

-- FILTERS

invert :: MonochromePixel -> MonochromePixel
invert =
  fmap (1 -)
{-# INLINE invert #-}

blackpoint :: Double -> MonochromePixel -> MonochromePixel
blackpoint x =
  fmap (\p -> p + ((1 - p) * x))
{-# INLINE blackpoint #-}

whitepoint :: Double -> MonochromePixel -> MonochromePixel
whitepoint x =
  fmap (\p -> p - (x * p))
{-# INLINE whitepoint #-}

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

logMsg :: MonadIO m => TimedFastLogger -> Text -> m ()
logMsg logger msg =
  liftIO $ logger (\time -> toLogStr time <> " | " <> toLogStr msg <> "\n")

tshow :: Show a => a -> Text
tshow =
  Text.pack . show
