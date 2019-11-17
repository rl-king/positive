{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import qualified Data.Massiv.Array as Array
import qualified Data.Massiv.Array.IO as Massiv
import Positive


main :: IO ()
main = do
  image <- readImage "assets/raw0001.png"
  print (Array.size image)
  print (Array.getComp image)
  print (Array.elemsCount image)
  defaultMain
    [ bench "process image" $ whnf process image
    , bench "process and encode png image" $ whnf (Massiv.encodeImage Massiv.imageWriteFormats "x.png" . process) image
    , bench "process and encode tif image" $ whnf (Massiv.encodeImage Massiv.imageWriteFormats "x.tif" . process) image
    , bench "process and encode jpg image" $ whnf (Massiv.encodeImage Massiv.imageWriteFormats "x.jpg" . process) image
    ]


process :: MonochromeImage Array.S -> MonochromeImage Array.S
process i =
    Array.compute $ processImage 1 1 1 1 i
