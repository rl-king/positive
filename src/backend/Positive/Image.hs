{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Image where

import Control.Exception.Safe (IOException, tryIO)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Massiv.Array.IO as Massiv
import Graphics.Image (Ix2 ((:.)))
import qualified Graphics.Image as HIP
import Positive.ImageSettings as ImageSettings
import Positive.Prelude hiding (ByteString)

-- IMAGE

type MonochromeImage =
  HIP.Image HIP.Y Word16

type MonochromeImageDouble =
  HIP.Image HIP.Y Double

type MonochromePixel =
  HIP.Pixel HIP.Y Word16

type MonochromePixelD =
  HIP.Pixel HIP.Y Double

-- LOAD

fromDisk :: FilePath -> IO (Either IOException MonochromeImage)
fromDisk =
  tryIO . fromDisk_

fromDisk_ :: FilePath -> IO MonochromeImage
fromDisk_ =
  HIP.readImageExact

fromDiskPreProcess :: Maybe Int -> ImageCrop -> String -> IO (Either IOException MonochromeImage)
fromDiskPreProcess targetSize imageCrop path = do
  let resize_ = maybe id resize targetSize
  fmap (resize_ . normalize . crop imageCrop) <$> fromDisk path

-- EDIT

applySettings :: ImageSettings -> MonochromeImage -> MonochromeImage
applySettings !is !image =
  let applyZones =
        foldr (\(z, v) acc -> acc . zone z v) id $
          filter
            ((/=) 0 . snd)
            [ (0.1, is.iZones.z1),
              (0.2, is.iZones.z2),
              (0.3, is.iZones.z3),
              (0.4, is.iZones.z4),
              (0.5, is.iZones.z5),
              (0.6, is.iZones.z6),
              (0.7, is.iZones.z7),
              (0.8, is.iZones.z8),
              (0.9, is.iZones.z9)
            ]
   in HIP.map
        ( fmap HIP.toWord16
            . applyZones
            . gamma is.iGamma
            . compress is.iBlackpoint is.iWhitepoint
            . invert
            . fmap HIP.toDouble
        )
        $ rotate is.iRotate image
{-# INLINE applySettings #-}

toDoubleI :: MonochromeImage -> MonochromeImageDouble
toDoubleI =
  HIP.map (fmap HIP.toDouble)
{-# INLINE toDoubleI #-}

fromDoubleI :: MonochromeImageDouble -> MonochromeImage
fromDoubleI =
  HIP.map (fmap HIP.toWord16)
{-# INLINE fromDoubleI #-}

resize :: Int -> MonochromeImage -> MonochromeImage
resize !targetWidth !image
  | HIP.cols image `div` targetWidth > 5 = fromDoubleI $ HIP.shrink2x2 (HIP.shrink3x3 (toDoubleI image))
  | HIP.cols image `div` targetWidth > 3 = fromDoubleI $ HIP.shrink3x3 (toDoubleI image)
  | otherwise = fromDoubleI $ HIP.shrink2x2 (toDoubleI image)
{-# INLINE resize #-}

crop :: ImageCrop -> MonochromeImage -> MonochromeImage
crop imageCrop image =
  let HIP.Sz2 h w = HIP.dims image
      (y, x) =
        ( floor $ int2Double h / 100 * imageCrop.icTop,
          floor $ int2Double w / 100 * imageCrop.icLeft
        )
      cropWidth = floor $ int2Double (w - x) * (imageCrop.icWidth / 100)
      cropHeight = floor $ int2Double cropWidth * mul
      mul = int2Double h / int2Double w
   in HIP.crop (const (y :. x, HIP.Sz2 cropHeight cropWidth)) image
{-# INLINE crop #-}

-- | Due to the nature of analog scans we resize the image to average the min and max values a bit
-- Not ideal, works for now
normalize :: MonochromeImage -> MonochromeImage
normalize image =
  let imageDouble = toDoubleI image
      !resized = HIP.shrink3x3 imageDouble
      !iMax = HIP.maxVal resized
      !iMin = HIP.minVal resized
   in fromDoubleI $
        HIP.map
          (fmap (\e -> (e - iMin) * ((HIP.maxValue - HIP.minValue) HIP.// (iMax - iMin)) + HIP.minValue))
          imageDouble
{-# INLINE normalize #-}

rotate :: Double -> MonochromeImage -> MonochromeImage
rotate !rad =
  case floor $ rad * 180 / pi :: Int of
    -90 -> HIP.rotate270
    -270 -> HIP.rotate90
    -180 -> HIP.rotate180
    90 -> HIP.rotate90
    180 -> HIP.rotate180
    270 -> HIP.rotate270
    _ -> id
{-# INLINE rotate #-}

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

-- FILTERS

invert :: MonochromePixelD -> MonochromePixelD
invert =
  fmap (1 -)
{-# INLINE invert #-}

compress :: Double -> Double -> MonochromePixelD -> MonochromePixelD
compress s l =
  fmap (\p -> min 1 . max 0 $ (p - s) / (l - s))
{-# INLINE compress #-}

gamma :: Double -> MonochromePixelD -> MonochromePixelD
gamma x =
  fmap (** x)
{-# INLINE gamma #-}

zone :: Double -> Double -> MonochromePixelD -> MonochromePixelD
zone t i =
  let m v = curve (1 - v - t)
   in fmap (\v -> v + (m v * i))
{-# INLINE zone #-}

curve :: Double -> Double
curve x =
  negate 1 / 2 * (cos (pi * x) - 1)
{-# INLINE curve #-}

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
