module Positive.Contacts where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Graphics.Image as HIP
import Positive.Image
import Positive.Prelude hiding (ByteString)
import Positive.Settings as Settings
import System.Directory
import System.FilePath.Posix

-- CONTACT

run :: IO ()
run = do
  missing <- findMissingContacts
  Text.putStrLn $ Text.unwords ["Found", tshow $ length missing, "missing contacts"]
  generateContacts missing

-- FIND

findMissingContacts :: IO [FilePath]
findMissingContacts =
  let check x = if isEmpty x then Nothing else Just x
   in do
        dirs <- fmap dropFileName <$> findImageSettingFiles
        catMaybes
          <$> mapM (\dir -> (dir <$) . check <$> diffedPreviewSettings (dir </> "previews") (dir </> "contacts")) dirs

-- WRITE

generateContacts :: [FilePath] -> IO ()
generateContacts dirs =
  let total = length dirs
   in for_ (zip dirs [1 .. total]) $ \(dir, index) -> do
        let output = dir </> "contacts" </> "contacts.jpg"
            imageContactSettings = dir </> "contacts" </> "image-settings.json"
        filenames <-
          fmap (\x -> dir </> "previews" </> x) . filter (isExtensionOf ".jpg")
            <$> listDirectory (dir </> "previews")
        HIP.writeImage output =<< toContacts filenames
        Aeson.encodeFile imageContactSettings empty
        Text.putStrLn $
          Text.unwords
            ["Generated contacts", Text.pack output, "|", tshow index, "of", tshow total]
