module Positive.ContactSheet where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Graphics.Image as HIP
import Positive.Image
import Positive.Prelude hiding (ByteString)
import Positive.Settings as Settings
import System.FilePath.Posix

-- CONTACT

run :: IO ()
run = do
  missing <- findMissingPreviews
  Text.putStrLn $ Text.unwords ["Found", tshow $ length missing, "missing previews"]
  -- generatePreviews missing
  print missing

-- FIND

findMissingPreviews :: IO [FilePath]
findMissingPreviews =
  let check x = if isEmpty x then Nothing else Just x
   in do
        dirs <- fmap dropFileName <$> findImageSettingFiles
        catMaybes
          <$> mapM (\dir -> (dir <$) . check <$> diffedPreviewSettings (dir </> "contacts") (dir </> "previews")) dirs

prependDir :: FilePath -> FilmRollSettings -> [(FilePath, ImageSettings)]
prependDir dir settings =
  (\x -> (dir </> Text.unpack (iFilename x), x)) <$> toList settings

-- WRITE

generatePreviews :: [(FilePath, ImageSettings)] -> IO ()
generatePreviews iss =
  let total = length iss
   in for_ (zip iss [1 .. total]) $ \((input, is), index) -> do
        let dir = dropFileName input
            output = dir </> "previews" </> replaceExtension (Text.unpack (iFilename is)) ".jpg"
        maybeImage <- readImageFromDisk input
        case maybeImage of
          Left _ ->
            Text.putStrLn $
              Text.unwords ["Error generating preview", Text.pack output, "|", tshow index, "of", tshow total]
          Right image -> do
            HIP.writeImage output $ processImage is (resizeImage 750 image)
            insertPreviewSettings (dir </> "previews" </> "image-settings.json") is
            Text.putStrLn $
              Text.unwords
                ["Generated preview", Text.pack output, "|", tshow index, "of", tshow total]
