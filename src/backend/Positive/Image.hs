{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Image where

import Control.Exception.Safe (IOException, tryIO)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Massiv.Array.IO as Massiv
import GHC.Float (int2Double)
import Graphics.Image (Ix2 ((:.)))
import qualified Graphics.Image as HIP
import Positive.ImageSettings as ImageSettings
import Positive.Prelude hiding (ByteString)

-- IMAGE

type MonochromeImage =
  HIP.Image HIP.Y Double

type MonochromePixel =
  HIP.Pixel HIP.Y Double

readImageFromDisk :: String -> IO (Either IOException MonochromeImage)
readImageFromDisk path =
  tryIO $ HIP.readImageY path

resizeImage :: Int -> MonochromeImage -> MonochromeImage
resizeImage targetWidth image =
  let mul = int2Double (HIP.rows image) / int2Double (HIP.cols image)
      preWidth = HIP.rows image - floor (int2Double targetWidth / 2)
      resize = HIP.resizeDW (HIP.Bicubic (-0.25)) HIP.Edge
   in resize (HIP.Sz2 (round (int2Double targetWidth * mul)) targetWidth) $
        resize (HIP.Sz2 (round (int2Double preWidth * mul)) preWidth) image

processImage :: ImageSettings -> MonochromeImage -> MonochromeImage
processImage is image =
  let HIP.Sz2 h w = HIP.dims image
      (y, x) =
        ( floor $ int2Double h / 100 * icTop is.iCrop,
          floor $ int2Double w / 100 * icLeft is.iCrop
        )
      cropWidth = floor $ int2Double (w - x) * (icWidth is.iCrop / 100)
      cropHeight = floor $ int2Double cropWidth * mul
      mul = int2Double h / int2Double w
   in HIP.map
        ( zone 0.9 is.iZone9
            . zone 0.5 is.iZone5
            . zone 0.1 is.iZone1
            . gamma is.iGamma
            . compress is.iBlackpoint is.iWhitepoint
            . invert
        )
        -- NOTE: 'normalize' is the only "automatic" correction and has a subtle effect
        -- at which it makes smaller images have more contrast due to different max and min
        -- values vs the original size
        . HIP.normalize
        . rotate is.iRotate
        $ HIP.crop (const (y :. x, HIP.Sz2 cropHeight cropWidth)) image

rotate :: Double -> MonochromeImage -> MonochromeImage
rotate rad =
  case floor $ rad * 180 / pi :: Int of
    -90 -> HIP.rotate270
    -270 -> HIP.rotate90
    -180 -> HIP.rotate180
    90 -> HIP.rotate90
    180 -> HIP.rotate180
    270 -> HIP.rotate270
    _ -> id

encode ::
  ( MonadIO m,
    Massiv.ColorSpace (HIP.DefSpace cs) i e,
    Massiv.ColorSpace (Massiv.BaseSpace (HIP.DefSpace cs)) i e
  ) =>
  FilePath ->
  HIP.Image cs e ->
  m ByteString
encode path image =
  liftIO . Massiv.encodeImageM Massiv.imageWriteAutoFormats path $
    HIP.unImage (HIP.toDefSpace image)

-- CONTACTS

toContacts :: [FilePath] -> IO MonochromeImage
toContacts images = do
  xs <- mapM toFilmStripRow (toColumns 6 images)
  let targetWidth = maximum $ HIP.cols <$> xs
  pure . foldr1 HIP.topToBottom $
    fmap (HIP.canvasSize (HIP.Fill 0.1) (\(HIP.Sz2 h _) -> (0 :. 0, HIP.Sz2 h targetWidth))) xs

toFilmStripRow :: [FilePath] -> IO MonochromeImage
toFilmStripRow images =
  let mul i = int2Double (HIP.cols i) / int2Double (HIP.rows i)
      resize i =
        HIP.resizeDW (HIP.Bicubic (-0.25)) HIP.Edge (HIP.Sz2 350 (round (mul i * 350))) i
   in foldr1 HIP.leftToRight <$> traverse (fmap resize . HIP.readImageY) images

toColumns :: Int -> [a] -> [[a]]
toColumns n xs =
  case splitAt n xs of
    (ys, zs@(_ : _)) -> ys : toColumns n zs
    (ys, _) -> [ys]

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
  let m v = curve (1 - v - t)
   in fmap (\v -> v + (m v * i))
{-# INLINE zone #-}

curve :: Double -> Double
curve x =
  negate 1 / 2 * (cos (pi * x) - 1)

-- DEBUG

draw :: Double -> Double -> IO ()
draw t i =
  traverse_
    (putStrLn . concat)
    [ flip replicate "=" . round . abs . (*) 50 . (-) x $
        zone_ t i x
      | x <- [0, 0.025 .. 1]
    ]

zone_ :: Double -> Double -> Double -> Double
zone_ t i v =
  let m = curve (1 - v - t)
   in (v + (m * i))
