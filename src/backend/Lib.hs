{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Lib (server) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Massiv.Array as Array
import qualified Data.Massiv.Array.IO as Massiv
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.ColorSpace as ColorSpace
import Network.Wai.Handler.Warp
import Servant
import System.IO


type Api =
  -- IMAGE
  "image"
  :> QueryParam' '[Required, Strict] "path" Text
  :> QueryParam' '[Required, Strict] "gamma" Double
  :> QueryParam' '[Required, Strict] "zone" Double
  :> Get '[OctetStream] BS.ByteString :<|>
  -- COORDINATE
  "image" :> "coordinate"
  :> QueryParam' '[Required, Strict] "path" Text
  :> QueryParam' '[Required, Strict] "gamma" Double
  :> QueryParam' '[Required, Strict] "zone" Double
  :> ReqBody '[JSON] (Int, Int) :> Post '[JSON] Double :<|>
  -- RAW
  Raw


api :: Proxy Api
api = Proxy


server :: IO ()
server =
  runSettings settings (serve api handlers )
  where
    settings =
      setPort 8080 $
      setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show @Int 8080))
      defaultSettings


handlers :: Server Api
handlers =
  handleImage :<|>
  handleCoordinate :<|>
  serveDirectoryFileServer "./"


handleImage :: Text -> Double -> Double -> Servant.Handler BS.ByteString
handleImage path g z = do
  image <- liftIO $ readImage (Text.unpack path)
  pure $ Massiv.encodeImage Massiv.imageWriteFormats (Text.unpack path) $
    Array.map (zone z . gamma g . invert) image


handleCoordinate :: Text -> Double -> Double -> (Int, Int) -> Servant.Handler Double
handleCoordinate path g z (x, y) = do
  image <- liftIO $ readImage (Text.unpack path)
  let image2 = Array.compute (Array.map (zone z . gamma g . invert) image) :: MonochromeImage Array.S
  case Array.index image2 (Array.Ix2 y x) of
    Just (ColorSpace.PixelY v) -> pure v
    Nothing -> pure 0


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


gamma :: Double -> MonochromePixel -> MonochromePixel
gamma x =
  fmap (** x)


zone :: Double -> MonochromePixel -> MonochromePixel
zone i =
  let
    m v = 1 - abs (v - 0.35)
  in
  fmap (\v -> v + (i * m v))
