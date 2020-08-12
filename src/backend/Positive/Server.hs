{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Positive.Server where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as BS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Image as HIP
import qualified Network.HTTP.Media as Media
import Network.Wai.Handler.Warp
import Positive.Settings
import Servant
import System.Directory
import qualified System.FilePath.Posix as Path
import System.Log.FastLogger (TimedFastLogger, toLogStr)

-- STATE

newtype State
  = State (IORef (Maybe (Text, MonochromeImage HIP.RPU)))

-- API

type Api =
  ImageApi :<|> SettingsApi

type ImageApi =
  "image" :> QueryParam' '[Required, Strict] "image-settings" ImageSettings :> Get '[Image] BS.ByteString
    :<|> "directory" :> QueryParam' '[Required, Strict] "dir" Text :> Get '[JSON] [Text]
    :<|> Raw

type SettingsApi =
  "image" :> "settings" :> Get '[JSON] ImageSettings

-- SERVER

server :: TimedFastLogger -> IO ()
server logger =
  let settings =
        setPort 8080 $
          setBeforeMainLoop
            (logMsg logger ("listening on port " <> tshow @Int 8080))
            defaultSettings
   in do
        ref <- newIORef Nothing
        runSettings settings (serve (Proxy @Api) (handlers logger (State ref)))

-- HANDLERS

handlers :: TimedFastLogger -> State -> Server Api
handlers logger state =
  ( handleImage logger state
      :<|> handleDirectory
      :<|> serveDirectoryFileServer "./"
  )
    :<|> undefined

handleImage :: TimedFastLogger -> State -> ImageSettings -> Servant.Handler BS.ByteString
handleImage logger state imageSettings = do
  image <- getImage logger state (iPath imageSettings)
  logMsg logger $ "Size: " <> tshow (HIP.dims image)
  logMsg logger "Processing image"
  pure
    . HIP.encode HIP.PNG []
    . HIP.exchange HIP.VS
    $ processImage imageSettings image

handleDirectory :: Text -> Servant.Handler [Text]
handleDirectory dir = do
  files <- liftIO $ listDirectory (Text.unpack dir)
  pure $ Text.pack <$> filter (\p -> Path.takeExtension p == ".png") files

getImage :: MonadIO m => TimedFastLogger -> State -> Text -> m (MonochromeImage HIP.RPU)
getImage logger state@(State ref) path = do
  maybeImage <- liftIO $ readIORef ref
  case maybeImage of
    Nothing -> do
      logMsg logger "Reading image"
      image <- liftIO $ readImageFromDisk (Text.unpack path)
      logMsg logger "Read image"
      liftIO $ modifyIORef' ref (const (Just (path, image)))
      logMsg logger "Updated IORef"
      pure image
    Just (cachedPath, cachedImage')
      | path == cachedPath -> logMsg logger "From cache image" >> pure cachedImage'
      | otherwise -> liftIO $ modifyIORef' ref (const Nothing) >> getImage logger state path

-- IMAGE

type MonochromeImage arr =
  HIP.Image arr HIP.Y Double

type MonochromePixel =
  HIP.Pixel HIP.Y Double

readImageFromDisk :: String -> IO (MonochromeImage HIP.RPU)
readImageFromDisk path =
  HIP.resize HIP.Bilinear HIP.Edge (900, 600)
    . HIP.resize HIP.Bilinear HIP.Edge (1800, 1200)
    <$> HIP.readImageY HIP.RPU path

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

instance MimeRender Image BS.ByteString where
  mimeRender _ = id

-- LOG

logMsg :: MonadIO m => TimedFastLogger -> Text -> m ()
logMsg logger msg =
  liftIO $ logger (\time -> toLogStr time <> " | " <> toLogStr msg <> "\n")

tshow :: Show a => a -> Text
tshow =
  Text.pack . show
