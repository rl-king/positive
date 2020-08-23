module Positive.Image where

import GHC.Float (int2Double)
import qualified Graphics.Image as HIP
import Positive.Prelude hiding (ByteString)
import Positive.Settings as Settings

-- IMAGE

type MonochromeImage arr =
  HIP.Image arr HIP.Y Double

type MonochromePixel =
  HIP.Pixel HIP.Y Double

readImageFromDisk :: String -> IO (MonochromeImage HIP.VU)
readImageFromDisk =
  HIP.readImageY HIP.VU

resizeImage :: Int -> MonochromeImage HIP.VU -> MonochromeImage HIP.VU
resizeImage targetWidth image =
  let mul = int2Double (HIP.rows image) / int2Double (HIP.cols image)
      preSize =
        if HIP.rows image - targetWidth > 3000
          then HIP.resize (HIP.Bicubic 0.25) HIP.Edge (round (int2Double 2500 * mul), 2500)
          else id
   in HIP.resize (HIP.Bicubic (-0.25)) HIP.Edge (round (int2Double targetWidth * mul), targetWidth) $
        preSize image

processImage :: ImageSettings -> MonochromeImage HIP.VU -> MonochromeImage HIP.VU
processImage is image =
  let (h, w) = HIP.dims image
      cropOffset =
        ( floor $ int2Double h / 100 * icTop (iCrop is),
          floor $ int2Double w / 100 * icLeft (iCrop is)
        )
      cropWidth = floor $ int2Double (w - snd cropOffset) * (icWidth (iCrop is) / 100)
      cropHeight = floor $ int2Double cropWidth * mul
      mul = int2Double h / int2Double w
   in HIP.map
        ( zone 0.9 (iZone9 is)
            . zone 0.5 (iZone5 is)
            . zone 0.1 (iZone1 is)
            . gamma (iGamma is)
            . compress (iBlackpoint is) (iWhitepoint is)
            . invert
        )
        -- NOTE: 'normalize' is the only "automatic" correction and has a subtle effect
        -- at which it makes smaller images have more contrast due to different max and min
        -- values vs the original size
        . HIP.normalize
        . (if iRotate is == 0 then id else HIP.rotate (HIP.Bicubic (-0.25)) (HIP.Fill 0) (iRotate is))
        $ HIP.crop cropOffset (cropHeight, cropWidth) image

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
  let m v = (1 - abs (v - t)) ^ (2 :: Int)
   in fmap (\v -> v + (i * m v))
{-# INLINE zone #-}
