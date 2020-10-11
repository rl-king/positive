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
  HIP.Image HIP.Y Double

type MonochromePixel =
  HIP.Pixel HIP.Y Double

-- LOAD

fromDisk :: String -> IO (Either IOException MonochromeImage)
fromDisk =
  tryIO . HIP.readImageY

fromDiskPreProcess :: Maybe Int -> ImageCrop -> String -> IO (Either IOException MonochromeImage)
fromDiskPreProcess targetSize imageCrop path = do
  let resize_ = maybe id resize targetSize
  image <- fromDisk path
  pure $ fmap (resize_ . normalize . crop imageCrop) image

-- EDIT

resize :: Int -> MonochromeImage -> MonochromeImage
resize !targetWidth !image
  | HIP.cols image `div` targetWidth > 5 = HIP.shrink2x2 (HIP.shrink3x3 image)
  | HIP.cols image `div` targetWidth > 3 = HIP.shrink3x3 image
  | otherwise = HIP.shrink2x2 image

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
   in HIP.map (applyZones . gamma is.iGamma . compress is.iBlackpoint is.iWhitepoint . invert) $ rotate is.iRotate image

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

-- | Due to the nature of analog scans we blur the image to average the min and max values a bit
-- Not ideal, works for now
normalize :: MonochromeImage -> MonochromeImage
normalize img =
  let !res = HIP.averageBlur5x5 HIP.Edge img
      !iMax = HIP.maxVal res
      !iMin = HIP.minVal res
   in HIP.map (fmap (\e -> (e - iMin) * ((HIP.maxValue - HIP.minValue) HIP.// (iMax - iMin)) + HIP.minValue)) img
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
      resizeBic i =
        HIP.resizeDW (HIP.Bicubic (-0.25)) HIP.Edge (HIP.Sz2 350 (round (mul i * 350))) i
   in foldr1 HIP.leftToRight <$> traverse (fmap resizeBic . HIP.readImageY) images

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
