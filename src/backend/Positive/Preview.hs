{-# LANGUAGE NumericUnderscores #-}

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
  generatePreviews log missing

loop :: MVar () -> (Text -> IO ()) -> IO ()
loop mvar log =
  void . forkIO . forever $
    takeMVar mvar >> threadDelay 2000_000_0 >> whenM (isEmptyMVar mvar) (run log) -- run if nothing happened in after 20 secs

-- FIND

findMissingPreviews :: IO [(FilePath, ImageSettings)]
findMissingPreviews =
  fmap concat . mapM (\dir -> prependDir dir <$> diffedPreviewSettings dir (dir </> "previews"))
    =<< fmap dropFileName <$> findImageSettingFiles

prependDir :: FilePath -> FilmRollSettings -> [(FilePath, ImageSettings)]
prependDir dir settings =
  (\x -> (dir </> Text.unpack (iFilename x), x)) <$> toList settings

-- WRITE

generatePreviews :: (Text -> IO ()) -> [(FilePath, ImageSettings)] -> IO ()
generatePreviews log iss =
  let total = length iss
   in for_ (zip iss [1 .. total]) $ \((input, is), index) -> do
        let dir = dropFileName input
            output = dir </> "previews" </> replaceExtension (Text.unpack (iFilename is)) ".jpg"
        maybeImage <- readImageFromDisk input
        case maybeImage of
          Left _ ->
            log $ Text.unwords ["Error generating preview", Text.pack output, "|", tshow index, "of", tshow total]
          Right image -> do
            HIP.writeImage output $ processImage is (resizeImage 750 image)
            insertPreviewSettings (dir </> "previews" </> "image-settings.json") is
            log $ Text.unwords ["Generated preview", Text.pack output, "|", tshow index, "of", tshow total]
