module Main where

import Criterion.Main
import qualified Graphics.Image as HIP
import qualified Positive.Image as Image
import qualified Positive.ImageSettings as ImageSettings
import Positive.Prelude

main :: IO ()
main =
  defaultMain
    [ env (HIP.shrink2x2 <$> HIP.readImageY "bench/medium.png") $ \img ->
        bgroup
          "With medium image"
          -- [ processingWithResize img,

          [ -- resizing img
            processing img
          ]
    ]

processing img =
  bgroup
    "Process"
    [ bench "Process image" $
        nf (Image.processImage (ImageSettings.plainImageSettings "")) img,
      bench "Process image blur" $
        nf (Image.processImage (ImageSettings.plainImageSettings "")) (HIP.averageBlur5x5 HIP.Edge img)
    ]

processingWithResize img =
  bgroup
    "Resize and process"
    [ bench "Full size" $
        nf (Image.processImage (ImageSettings.plainImageSettings "")) img,
      bench "1/4 size" $
        nf (Image.processImage (ImageSettings.plainImageSettings "") . HIP.shrink2x2) img
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
