{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Positive.Server where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BS
-- import qualified Data.HIP.Array as Array
-- import qualified Data.HIP.Array.IO as HIP
import Data.Text (Text)
import qualified Data.Text as Text
-- import qualified Graphics.ColorSpace as ColorSpace
import qualified Graphics.Image as HIP
import qualified Network.HTTP.Media as Media
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import qualified System.FilePath.Posix as Path
import System.IO

newtype State = State
  { _cachedImage :: TVar (Maybe (Text, MonochromeImage HIP.VS))
  }

type Api =
  -- IMAGE
  "image"
    :> QueryParam' '[Required, Strict] "path" Text
    :> QueryParam' '[Required, Strict] "gamma" Double
    :> QueryParam' '[Required, Strict] "zone-1" Double
    :> QueryParam' '[Required, Strict] "zone-5" Double
    :> QueryParam' '[Required, Strict] "zone-9" Double
    :> QueryParam' '[Required, Strict] "blackpoint" Double
    :> QueryParam' '[Required, Strict] "whitepoint" Double
    :> Get '[Image] BS.ByteString
      :<|>
      -- RAW
      "directory"
    :> QueryParam' '[Required, Strict] "dir" Text
    :> Get '[JSON] [Text]
      :<|>
      -- RAW
      Raw

server :: IO ()
server = do
  cachedImage <- newTVarIO Nothing
  runSettings settings (serve (Proxy @Api) (handlers (State cachedImage)))
  where
    settings =
      setPort 8080 $
        setBeforeMainLoop
          (hPutStrLn stderr ("listening on port " ++ show @Int 8080))
          defaultSettings

handlers :: State -> Server Api
handlers state =
  handleImage state
    :<|> handleDirectory
    :<|> serveDirectoryFileServer "./"

handleImage :: State -> Text -> Double -> Double -> Double -> Double -> Double -> Double -> Servant.Handler BS.ByteString
handleImage state path g z1 z5 z9 bp wp = do
  image <- liftIO $ getImage state path
  pure $ HIP.encode HIP.JPG [] $
    processImage g z1 z5 z9 bp wp image

handleDirectory :: Text -> Servant.Handler [Text]
handleDirectory dir = do
  files <- liftIO $ listDirectory (Text.unpack dir)
  pure $ Text.pack <$> filter (\p -> Path.takeExtension p == ".png") files

getImage :: State -> Text -> IO (MonochromeImage HIP.VS)
getImage (State cachedImage) path = do
  maybeImage <- readTVarIO cachedImage
  case maybeImage of
    Nothing -> do
      putStrLn "Read image"
      image <- readImage (Text.unpack path)
      atomically $ writeTVar cachedImage (Just (path, image))
      pure image
    Just (cachedPath, cachedImage')
      | path == cachedPath ->
        putStrLn "From cache image" >> pure cachedImage'
      | otherwise -> do
        putStrLn "Read image"
        imageNew <- readImage (Text.unpack path)
        atomically $ writeTVar cachedImage (Just (path, imageNew))
        pure imageNew

-- IMAGE

type MonochromeImage r =
  HIP.Image r HIP.Y Double

type MonochromePixel =
  HIP.Pixel HIP.Y Double

readImage :: String -> IO (MonochromeImage HIP.VS)
readImage =
  HIP.readImageY HIP.VS

processImage :: Double -> Double -> Double -> Double -> Double -> Double -> MonochromeImage HIP.VS -> MonochromeImage HIP.VS
processImage g z1 z5 z9 bp wp =
  HIP.map (whitepoint wp . blackpoint bp . zone 0.95 z9 . zone 0.5 z5 . zone 0.15 z1 . gamma g . invert)

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
