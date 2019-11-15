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
import qualified Graphics.ColorSpace as ColorSpace
import Network.Wai.Handler.Warp
import Servant
import System.IO


type Api =
  "image"
  :> QueryParam' '[Required, Strict] "gamma" Double
  :> Get '[OctetStream] BS.ByteString :<|>
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
  serveDirectoryFileServer "./"


handleImage :: Double -> Servant.Handler BS.ByteString
handleImage g = do
  image <- liftIO $ readImage "test2.png"
  pure $ Massiv.encodeImage Massiv.imageWriteFormats "image.png" $
    Array.map (gamma g . invert) image


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
