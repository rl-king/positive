module Positive.Init where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Positive.ImageSettings as ImageSettings
import Positive.Prelude hiding (ByteString)
import System.Directory
import System.FilePath.Posix

run :: Bool -> IO ()
run replace =
  let imageSettings = "image-settings.json"
      imagePreviewSettings = "previews" </> imageSettings
      check path = if replace then pure False else doesFileExist path
   in do
        filenames <-
          fmap Text.pack
            . filter (\x -> isExtensionOf ".tif" x || isExtensionOf ".png" x)
            <$> listDirectory "."
        unless (null filenames) $ do
          let settings = fromFilenames filenames
          unlessM (check imageSettings) $ Aeson.encodeFile imageSettings settings
          createDirectoryIfMissing False (dropFileName imagePreviewSettings)
          unlessM (check imagePreviewSettings) $ Aeson.encodeFile imagePreviewSettings empty
