{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.SingleImage where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Graphics.Image as HIP
import qualified Positive.Data.FilmRoll as FilmRoll
import Positive.Data.ImageSettings (ImageSettings)
import qualified Positive.Data.ImageSettings as ImageSettings
import qualified Positive.Data.Path as Path
import qualified Positive.Image as Image
import qualified Positive.Image.Util as Util
import Positive.Prelude hiding (ByteString)
import System.Directory
import System.FilePath.Posix

-- CONTACT

run :: FilePath -> IO ()
run filepath = do
  error "todo"

-- maybeSettings <-
--   join . rightToMaybe
--     <$> tryAny (Aeson.decodeFileStrict "image-settings.json")
-- let filename = Path.fromFilePath $ takeFileName filepath
-- case HashMap.lookup filename . FilmRoll.imageSettings =<< maybeSettings of
--   Nothing ->
--     generate log "No settings file found, generating plain image: " filepath $
--       ImageSettings.emptyImageSettings filename
--   Just settings ->
--     generate log "ImageSettings file found, generating image: " filepath settings

generate :: MonadIO m => (Text -> m ()) -> Text -> FilePath -> ImageSettings -> m ()
generate log message filepath is = do
  image <- Image.fromDiskPreProcess Nothing is.crop filepath
  liftIO $ createDirectoryIfMissing False (dropFileName filepath </> "highres")
  outputWithCount <-
    Util.ensureUniqueFilename $
      dropFileName filepath </> "highres" </> takeFileName filepath
  log $ message <> Text.pack outputWithCount
  either (log . tshow) (HIP.writeImage outputWithCount . Image.applySettings is) image
