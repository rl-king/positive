module Positive.Init where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Positive.ImageSettings as ImageSettings
import Positive.Prelude hiding (ByteString)
import System.Directory
import System.FilePath.Posix

run :: Bool -> IO ()
run overwrite =
  let filename = "image-settings.json"
      imageSettings = filename
      imagePreviewSettings = "previews" </> filename
      imageContactSettings = "contacts" </> filename
      check path = if overwrite then pure False else doesFileExist path
   in do
        filenames <- fmap Text.pack . filter (isExtensionOf ".png") <$> listDirectory "."
        unless (null filenames) $ do
          let settings = fromFilenames filenames
          unlessM (check imageSettings) $ Aeson.encodeFile imageSettings settings
          createDirectoryIfMissing False (dropFileName imagePreviewSettings)
          unlessM (check imagePreviewSettings) $ Aeson.encodeFile imagePreviewSettings empty
          createDirectoryIfMissing False (dropFileName imageContactSettings)
          unlessM (check imageContactSettings) $ Aeson.encodeFile imageContactSettings empty
