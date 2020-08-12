{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import qualified Data.Massiv.Array as Array
import qualified Data.Massiv.Array.IO as Massiv
import Data.Massiv.Array.Manifest.Vector as AV
import qualified Data.Vector.Storable as VS
import Data.Vector.Unboxed as UB
import qualified Graphics.ColorSpace as ColorSpace
import Graphics.Image (Bilinear(..), Border(..), Y, fromLists, resize)
import qualified Graphics.Image as Hip
import Graphics.Image.Interface as Interface
import Positive
import Unsafe.Coerce

-- resize Bilinear Edge (100, 640) frog
main :: IO ()
main = do
  image <- readImage "assets/raw0001.png"
  let (Array.Sz2 w h) = Array.size image
  print (Array.size image)
  print (Array.getComp image)
  print (Array.elemsCount image)
  Hip.writeImage "resized.png" (res (div w 10) (div h 10) image)
  defaultMain
    [ bench "process image" $ whnf process image
    , bench "to vector" $ whnf conv image
    , bench "to vector and resize" $ whnf (res (div w 10) (div h 10)) image


    -- , bench "process and encode png image" $ whnf (Massiv.encodeImage Massiv.imageWriteFormats "x.png" . process) image
    -- , bench "process and encode tif image" $ whnf (Massiv.encodeImage Massiv.imageWriteFormats "x.tif" . process) image
    -- , bench "process and encode jpg image" $ whnf (Massiv.encodeImage Massiv.imageWriteFormats "x.jpg" . process) image
    ]

res :: Int -> Int -> Massiv.Image Array.S ColorSpace.Y Double -> Hip.Image Hip.VU Y Double
res w h =
  resize Bilinear Edge (w, 640) . fromLists . conv


conv :: Massiv.Image Array.S ColorSpace.Y Double -> [[Hip.Pixel Hip.Y Double]]
conv =
  Array.toLists . Array.map unsafeCoerce


process :: MonochromeImage Array.S -> MonochromeImage Array.S
process i =
    Array.compute $ processImage 1 1 1 1 1 1 i
