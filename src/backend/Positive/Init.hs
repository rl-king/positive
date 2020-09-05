module Positive.Init where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Positive.Prelude hiding (ByteString)
import Positive.Settings as Settings
import System.Directory
import System.FilePath.Posix

run :: FilePath -> IO ()
run dir =
  let filename = "image-settings.json"
      imageSettings = dir </> filename
      imagePreviewSettings = dir </> "previews" </> filename
      imageContactSettings = dir </> "contacts" </> filename
   in do
        filenames <- fmap Text.pack . filter (isExtensionOf ".png") <$> listDirectory dir
        let settings = fromFilenames filenames
        unlessM (doesFileExist imageSettings) $ Aeson.encodeFile imageSettings settings
        createDirectoryIfMissing False (dropFileName imagePreviewSettings)
        unlessM (doesFileExist imagePreviewSettings) $ Aeson.encodeFile imagePreviewSettings empty
        createDirectoryIfMissing False (dropFileName imageContactSettings)
        unlessM (doesFileExist imageContactSettings) $ Aeson.encodeFile imageContactSettings empty
