{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Image where

import Control.Exception.Safe (try)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Massiv.Array.IO as Massiv
import Graphics.Image (Ix2 ((:.)))
import qualified Graphics.Image as HIP
import Positive.Data.ImageSettings
  ( ImageCrop,
    ImageSettings,
  )
import qualified Positive.Language as Language
import Positive.Prelude hiding (ByteString)

-- IMAGE

type Monochrome =
  HIP.Image HIP.Y Word16

type MonochromePixel =
  HIP.Pixel HIP.Y Word16

type MonochromeDouble =
  HIP.Image HIP.Y Double

type MonochromePixelDouble =
  HIP.Pixel HIP.Y Double

-- LOAD

fromDisk :: MonadIO m => FilePath -> m (Either SomeException Monochrome)
fromDisk =
  liftIO . try . fromDisk_

fromDisk_ :: MonadIO m => FilePath -> m Monochrome
fromDisk_ =
  HIP.readImageExact

fromDiskPreProcess ::
  MonadIO m =>
  Maybe Int ->
  ImageCrop ->
  String ->
  m (Either SomeException Monochrome)
fromDiskPreProcess targetSize imageCrop path =
  fmap (fromDoubleI . maybe identity resize targetSize . normalize . toDoubleI . crop imageCrop)
    <$> fromDisk path

-- EDIT

applySettings :: ImageSettings -> Monochrome -> Monochrome
applySettings !is !image =
  let applyZones =
        foldr (\(z, v) acc -> acc . zone z v) identity $
          filter
            ((/=) 0 . snd)
            [ (0.1, is.zones.z1),
              (0.2, is.zones.z2),
              (0.3, is.zones.z3),
              (0.4, is.zones.z4),
              (0.5, is.zones.z5),
              (0.6, is.zones.z6),
              (0.7, is.zones.z7),
              (0.8, is.zones.z8),
              (0.9, is.zones.z9)
            ]
      expressions =
        either (const identity) (foldr (.) identity) $
          traverse
            ( \e ->
                (\expr p -> Language.eval p e.eValue expr)
                  <$> Language.parse e.eExpr
            )
            is.expressions
   in HIP.map
        ( fmap HIP.toWord16
            . applyZones
            . fmap expressions
            . gamma is.gamma
            . compress is.blackpoint is.whitepoint
            . invert
            . fmap HIP.toDouble
        )
        $ rotate is.rotate image
{-# INLINE applySettings #-}

toDoubleI :: Monochrome -> MonochromeDouble
toDoubleI =
  HIP.map (fmap HIP.toDouble)
{-# INLINE toDoubleI #-}

fromDoubleI :: MonochromeDouble -> Monochrome
fromDoubleI =
  HIP.map (fmap HIP.toWord16)
{-# INLINE fromDoubleI #-}

resize :: Int -> MonochromeDouble -> MonochromeDouble
resize !targetWidth !image =
  case HIP.cols image `div` targetWidth of
    0 -> image
    1 -> image
    2 -> HIP.shrink2x2 image
    3 -> HIP.shrink3x3 image
    4 -> HIP.shrink2x2 (HIP.shrink2x2 image)
    _ -> HIP.shrink2x2 (HIP.shrink3x3 image)
{-# INLINE resize #-}

crop :: ImageCrop -> Monochrome -> Monochrome
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
normalize :: MonochromeDouble -> MonochromeDouble
normalize image =
  let !resized = HIP.shrink3x3 image
      !iMax = HIP.maxVal resized
      !iMin = HIP.minVal resized
   in HIP.map
        (fmap (\e -> (e - iMin) * ((HIP.maxValue - HIP.minValue) HIP.// (iMax - iMin)) + HIP.minValue))
        image
{-# INLINE normalize #-}

rotate :: Double -> Monochrome -> Monochrome
rotate !rad =
  case floor $ rad * 180 / pi :: Int of
    -90 -> HIP.rotate270
    -270 -> HIP.rotate90
    -180 -> HIP.rotate180
    90 -> HIP.rotate90
    180 -> HIP.rotate180
    270 -> HIP.rotate270
    _ -> identity
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

invert :: MonochromePixelDouble -> MonochromePixelDouble
invert =
  fmap (1 -)
{-# INLINE invert #-}

compress :: Double -> Double -> MonochromePixelDouble -> MonochromePixelDouble
compress s l =
  fmap (\p -> min 1 . max 0 $ (p - s) / (l - s))
{-# INLINE compress #-}

gamma :: Double -> MonochromePixelDouble -> MonochromePixelDouble
gamma x =
  fmap (** x)
{-# INLINE gamma #-}

zone :: Double -> Double -> MonochromePixelDouble -> MonochromePixelDouble
zone t i =
  let m v = curve (1 - v - t)
   in fmap (\v -> v + m v * i)
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
   in (v + m * i)
