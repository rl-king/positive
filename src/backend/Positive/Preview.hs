{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Preview where

import Control.Concurrent.MVar
import qualified Data.Text as Text
import qualified Graphics.Image as HIP
import Positive.Image
import Positive.ImageSettings as ImageSettings
import Positive.Prelude hiding (ByteString)
import System.FilePath.Posix

-- PREVIEW

run :: (Text -> IO ()) -> IO ()
run log = do
  missing <- findMissingPreviews
  log $ Text.unwords ["Found", tshow $ length missing, "outdated preview(s)"]
  for_ (zip missing [1 :: Int ..]) $ \(x, index) ->
    generatePreview (\t -> log (t <> " | " <> tshow index <> " of " <> tshow (length missing))) x

loop :: MVar [(FilePath, ImageSettings)] -> (Text -> IO ()) -> IO ()
loop mvar log =
  void . forkIO . forever $ do
    work <- takeMVar mvar
    case work of
      [] -> log "Finished generating previews"
      x : xs -> generatePreview log x >> void (tryPutMVar mvar xs)

-- FIND

findMissingPreviews :: IO [(FilePath, ImageSettings)]
findMissingPreviews =
  fmap concat . mapM (\dir -> prependDir dir <$> diffedPreviewSettings dir (dir </> "previews"))
    =<< fmap dropFileName <$> findImageSettingFiles

prependDir :: FilePath -> FilmRollSettings -> [(FilePath, ImageSettings)]
prependDir dir settings =
  (\x -> (dir </> Text.unpack x.iFilename, x)) <$> toList settings

-- WRITE

generatePreview :: (Text -> IO ()) -> (FilePath, ImageSettings) -> IO ()
generatePreview log (input, is) = do
  let dir = dropFileName input
      output = dir </> "previews" </> replaceExtension (Text.unpack is.iFilename) ".jpg"
  maybeImage <- readImageFromDisk input
  case maybeImage of
    Left _ ->
      log $ Text.unwords ["Error generating preview", Text.pack output]
    Right image -> do
      HIP.writeImage output $ processImage is (resizeImage 750 image)
      insertPreviewSettings (dir </> "previews" </> "image-settings.json") is
      log $ Text.unwords ["Generated preview", Text.pack output]
