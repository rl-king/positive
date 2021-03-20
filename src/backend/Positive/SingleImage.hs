{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.SingleImage where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Graphics.Image as HIP
import qualified Positive.Filename as Filename
import qualified Positive.FilmRoll as FilmRoll
import qualified Positive.Image as Image
import Positive.Image.Settings as Settings
import qualified Positive.Image.Util as Util
import Positive.Prelude hiding (ByteString)
import System.Directory
import System.FilePath.Posix

-- CONTACT

run :: (Text -> IO ()) -> FilePath -> IO ()
run log filepath = do
  maybeSettings <-
    join . rightToMaybe
      <$> tryAny (Aeson.decodeFileStrict "image-settings.json")
  let filename = Filename.fromFilePath $ takeFileName filepath
  case HashMap.lookup filename . FilmRoll.frsSettings =<< maybeSettings of
    Nothing ->
      generate log "No settings file found, generating plain image: " filepath $
        FilmRoll.plainImageSettings filename
    Just settings ->
      generate log "Settings file found, generating image: " filepath settings

generate :: MonadIO m => (Text -> m ()) -> Text -> FilePath -> Settings -> m ()
generate log message filepath is = do
  image <- Image.fromDiskPreProcess Nothing is.iCrop filepath
  liftIO $ createDirectoryIfMissing False (dropFileName filepath </> "highres")
  outputWithCount <-
    Util.ensureUniqueFilename $
      dropFileName filepath </> "highres" </> takeFileName filepath
  log $ message <> Text.pack outputWithCount
  either (log . tshow) (HIP.writeImage outputWithCount . Image.applySettings is) image
