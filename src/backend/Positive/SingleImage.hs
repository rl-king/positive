{-# LANGUAGE ScopedTypeVariables #-}

module Positive.SingleImage where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Graphics.Image as HIP
import Positive.Image
import Positive.ImageSettings as ImageSettings
import Positive.Prelude hiding (ByteString)
import System.Directory
import System.FilePath.Posix

-- CONTACT

run :: (Text -> IO ()) -> FilePath -> IO ()
run log filepath = do
  maybeSettings <- join . rightToMaybe <$> tryAny (Aeson.decodeFileStrict "image-settings.json")
  let filename = Text.pack $ takeFileName filepath
  case HashMap.lookup filename . frsSettings =<< maybeSettings of
    Nothing ->
      generate log "No settings file found, generating plain image" filepath $
        plainImageSettings filename
    Just settings ->
      generate log "Settings file found, generating image" filepath settings

generate :: (Text -> IO ()) -> Text -> String -> ImageSettings -> IO ()
generate log message filepath settings = do
  log message
  image <- readImageFromDisk filepath
  createDirectoryIfMissing False "highres"
  either (log . tshow) (HIP.writeImage ("highres" </> filepath) . processImage settings) image
