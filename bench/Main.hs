{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import qualified Graphics.Image as HIP
import qualified Positive.Image as Image
import qualified Positive.Data.ImageSettings as Settings
import Positive.Prelude

main :: IO ()
main =
  defaultMain
    [ env (Image.fromDisk_ "bench/large.tif") $ \img ->
        processing img
    ]

processing img =
  bgroup
    "Process"
    [ -- bench "Process image" $
      --nf (Image.applySettings (Settings.plainImageSettings "")) img,
      bench "Process image blur" $ --4.5s
        nf (Image.applySettings (Settings.plainImageSettings "") . Image.normalize) img
    ]

processingWithResize img =
  bgroup
    "Resize and process"
    [ bench "Full size" $
        nf (Image.applySettings (Settings.plainImageSettings "")) img,
      bench "1/4 size" $
        nf (Image.applySettings (Settings.plainImageSettings "")) img
    ]

resizing img =
  bgroup
    "Effect of resize composition"
    [ bench "One resize" $
        nf HIP.shrink2x2 img,
      bench "Triple resize" $
        nf (HIP.shrink2x2 . HIP.shrink2x2 . HIP.shrink2x2) img,
      bench "Bicubic resize" $
        nf (HIP.resizeDW (HIP.Bicubic (-0.25)) HIP.Edge (HIP.Sz2 1000 1000)) img
    ]
