module Positive.Preview where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Graphics.Image as HIP
import Positive.Image
import Positive.Prelude hiding (ByteString)
import Positive.Settings as Settings
import System.Directory
import System.FilePath.Posix

-- PREVIEW

run :: IO ()
run = do
  missing <- findMissingPreviews
  print $ Text.unwords ["Found", tshow $ length missing, "missing previews"]
  generatePreviews missing

-- FIND

findMissingPreviews :: IO [(FilePath, ImageSettings)]
findMissingPreviews =
  fmap concat . mapM (\dir -> prependDir dir <$> diffedPreviewSettings dir)
    =<< fmap dropFileName <$> findImageSettingFiles

prependDir :: FilePath -> FilmRollSettings -> [(FilePath, ImageSettings)]
prependDir dir settings =
  (\x -> (dir </> Text.unpack (iFilename x), x)) <$> toList settings

-- DIFF

diffedPreviewSettings :: FilePath -> IO FilmRollSettings
diffedPreviewSettings dir =
  let rs = dir </> "image-settings.json"
      ps = dir </> "previews" </> "image-settings.json"
   in do
        exists <- doesPathExist ps
        unless exists $ Aeson.encodeFile ps empty
        rollSettings <- Aeson.decodeFileStrict rs
        previewSettings <- Aeson.decodeFileStrict ps
        maybe (fail "Something went wrong decoding the settings files") pure $
          Settings.difference <$> rollSettings <*> previewSettings

-- WRITE

generatePreviews :: [(FilePath, ImageSettings)] -> IO ()
generatePreviews iss =
  let total = length iss
   in for_ (zip iss [1 .. total]) $ \((input, is), index) -> do
        let dir = dropFileName input
            output = dir </> "previews" </> replaceExtension (Text.unpack (iFilename is)) ".jpg"
        maybeImage <- liftIO $ readImageFromDisk input
        case maybeImage of
          Left _ ->
            print $
              Text.unwords ["Error generating preview", Text.pack output, "|", tshow index, "of", tshow index]
          Right image -> do
            liftIO . HIP.writeImage output $ processImage is (resizeImage 750 image)
            print $
              Text.unwords
                ["Generated preview", Text.pack output, "|", tshow index, "of", tshow index]

insertPreviewSettings :: FilePath -> ImageSettings -> IO ()
insertPreviewSettings ps settings = do
  previewSettings <- Aeson.decodeFileStrict ps
  maybe
    (fail "Something went wrong decoding the settings file")
    (Aeson.encodeFile ps . Settings.insert settings)
    previewSettings
