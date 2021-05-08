module Positive.Init where

import qualified Data.Aeson as Aeson
import qualified Positive.Data.Path as Path
import qualified Positive.Data.FilmRoll as FilmRoll
import Positive.Prelude hiding (ByteString)
import System.Directory
import System.FilePath.Posix

run :: Bool -> IO ()
run replace =
  error "todo"

-- let imageSettings = "image-settings.json"
--     imagePreviewSettings = "previews" </> imageSettings
--     check path = if replace then pure False else doesFileExist path
--  in do
--       filenames <-
--         fmap Path.fromFilePath
--           . filter (\x -> isExtensionOf ".tif" x || isExtensionOf ".png" x)
--           <$> listDirectory "."
--       unless (null filenames) $ do
--         let settings = FilmRoll.fromFilenames filenames
--         unlessM (check imageSettings) $ Aeson.encodeFile imageSettings settings
--         createDirectoryIfMissing False (dropFileName imagePreviewSettings)
--         unlessM (check imagePreviewSettings) $
--           Aeson.encodeFile imagePreviewSettings FilmRoll.empty
