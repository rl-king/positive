module Main where

import Data.Foldable
import qualified Data.Massiv.Array.IO as Massiv
import qualified Graphics.Image as Image
import Positive.Server
import System.Directory
import System.Environment
import qualified System.FilePath.Posix as Path

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["run", dir] -> do
      files <- filter ((".png" ==) . Path.takeExtension) <$> listDirectory dir
      print files
      for_ files $
        \path -> do
          let path_ x = dir Path.</> x <> path
          image <- readImage $ path_ ""
          Image.writeImageExact (path_ "positive-") $
            processImage 2.2 0 0 0 0 0 image
          print path
    _ ->
      server
