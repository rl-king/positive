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
import Control.Exception.Safe (tryIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
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
  { iaImage :: route :- "image" :> QueryParam' '[Required, Strict] "image-settings" ImageSettings :> Get '[Image] ByteString,
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

handleImage :: TimedFastLogger -> LoadedImage -> ImageSettings -> Servant.Handler ByteString
handleImage logger state imageSettings = do
  image <- getImage logger state (iPath imageSettings)
  logMsg logger "Processing image"
  pure
    . HIP.encode HIP.PNG []
    . HIP.exchange HIP.VS
    $ processImage imageSettings image

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

getImage :: MonadIO m => TimedFastLogger -> LoadedImage -> Text -> m (MonochromeImage HIP.RPU)
getImage logger state@(LoadedImage ref) path = do
  maybeImage <- liftIO $ tryReadMVar ref
  case maybeImage of
    Nothing -> do
      logMsg logger "Reading image"
      image <- liftIO $ resizeImage <$> readImageFromDisk (Text.unpack path)
      logMsg logger "Read image"
      liftIO $ putMVar ref (path, HIP.exchange HIP.VU image)
      logMsg logger "Updated MVar"
      pure image
    Just (cachedPath, cachedImage')
      | path == cachedPath -> do
        logMsg logger "From MVar image"
        pure (HIP.exchange HIP.RPU cachedImage')
      | otherwise -> liftIO $ getImage logger state path

-- IMAGE

type MonochromeImage arr =
  HIP.Image arr HIP.Y Double

type MonochromePixel =
  HIP.Pixel HIP.Y Double

readImageFromDisk :: String -> IO (MonochromeImage HIP.RPU)
readImageFromDisk =
  HIP.readImageY HIP.RPU

resizeImage :: MonochromeImage HIP.RPU -> MonochromeImage HIP.RPU
resizeImage =
  HIP.resize HIP.Bilinear HIP.Edge (900, 600) . HIP.resize HIP.Bilinear HIP.Edge (1800, 1200)

processImage :: ImageSettings -> MonochromeImage HIP.RPU -> MonochromeImage HIP.RPU
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
    "image" Media.// "png"

instance MimeRender Image ByteString where
  mimeRender _ = id

-- LOG

logMsg :: MonadIO m => TimedFastLogger -> Text -> m ()
logMsg logger msg =
  liftIO $ logger (\time -> toLogStr time <> " | " <> toLogStr msg <> "\n")

tshow :: Show a => a -> Text
tshow =
  Text.pack . show
