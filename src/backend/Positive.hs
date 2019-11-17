{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Positive where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Massiv.Array as Array
import qualified Data.Massiv.Array.IO as Massiv
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.ColorSpace as ColorSpace
import qualified Network.HTTP.Media as Media
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import qualified System.FilePath.Posix as Path
import System.IO


newtype State =
  State
  { _cachedImage :: TVar (Maybe (Text, MonochromeImage Array.S))
  }


data Image


instance Accept Image where
    contentType _ =
      "image" Media.// "png"


instance MimeRender Image BS.ByteString where
    mimeRender _ = id


type Api =
  -- IMAGE
  "image"
  :> QueryParam' '[Required, Strict] "path" Text
  :> QueryParam' '[Required, Strict] "gamma" Double
  :> QueryParam' '[Required, Strict] "zone-1" Double
  :> QueryParam' '[Required, Strict] "zone-5" Double
  :> QueryParam' '[Required, Strict] "zone-9" Double
  :> Get '[Image] BS.ByteString :<|>
  -- COORDINATE
  "image" :> "coordinate"
  :> QueryParam' '[Required, Strict] "path" Text
  :> QueryParam' '[Required, Strict] "gamma" Double
  :> QueryParam' '[Required, Strict] "zone-1" Double
  :> QueryParam' '[Required, Strict] "zone-5" Double
  :> QueryParam' '[Required, Strict] "zone-9" Double
  :> ReqBody '[JSON] (Int, Int) :> Post '[JSON] Double :<|>
  -- RAW
  "directory"
  :> QueryParam' '[Required, Strict] "dir" Text
  :> Get '[JSON] [Text] :<|>
  -- RAW
  Raw


api :: Proxy Api
api = Proxy


server :: IO ()
server = do
  cachedImage <- newTVarIO Nothing
  runSettings settings (serve api (handlers (State cachedImage)))
  where
    settings =
      setPort 8080 $
      setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show @Int 8080))
      defaultSettings


handlers :: State -> Server Api
handlers state =
  handleImage state :<|>
  handleCoordinate state :<|>
  handleDirectory :<|>
  serveDirectoryFileServer "./"


handleImage :: State -> Text -> Double -> Double -> Double -> Double -> Servant.Handler BS.ByteString
handleImage state path g z1 z5 z9 = do
  image <- liftIO $ getImage state path
  pure $ Massiv.encodeImage Massiv.imageWriteFormats (Text.unpack path) $
    processImage g z1 z5 z9 image


handleCoordinate :: State -> Text -> Double -> Double -> Double -> Double -> (Int, Int) -> Servant.Handler Double
handleCoordinate state path g z1 z5 z9 (x, y) = do
  image <- liftIO $ getImage state path
  let image2 =
        Array.compute $ processImage g z1 z5 z9 image :: MonochromeImage Array.S
  case Array.index image2 (Array.Ix2 y x) of
    Just (ColorSpace.PixelY v) -> pure v
    Nothing -> pure 0


handleDirectory :: Text -> Servant.Handler [Text]
handleDirectory dir = do
  files <- liftIO $ listDirectory (Text.unpack dir)
  pure $ Text.pack <$> filter (\p -> Path.takeExtension p == ".png") files


processImage :: Double -> Double -> Double -> Double -> MonochromeImage Array.S -> MonochromeImage Array.D
processImage g z1 z5 z9 =
  Array.map (zone 0.95 z9 . zone 0.5 z5 . zone 0.15 z1 . gamma g . invert)


getImage :: State -> Text -> IO (MonochromeImage Array.S)
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
  Massiv.Image r ColorSpace.Y Double


type MonochromePixel =
  ColorSpace.Pixel ColorSpace.Y Double


readImage :: String -> IO (MonochromeImage Array.S)
readImage =
  Massiv.readImageAuto


invert :: MonochromePixel -> MonochromePixel
invert =
  fmap (1 -)
{-# INLINE invert #-}


gamma :: Double -> MonochromePixel -> MonochromePixel
gamma x =
  fmap (** x)
{-# INLINE gamma #-}


zone :: Double -> Double -> MonochromePixel -> MonochromePixel
zone t i =
  let
    m v = (1 - abs (v - t)) * (1 - abs (v - t))
  in
  fmap (\v -> v + (i * m v))
{-# INLINE zone #-}
