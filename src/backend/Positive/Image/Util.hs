{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Positive.Image.Util where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import qualified Data.Text as Text
import Positive.Data.FilmRoll
import Positive.Data.ImageSettings
import Positive.Prelude
import qualified System.FilePath.Glob as Glob
import System.FilePath.Posix as Path

-- FS

findImageSettings :: MonadIO m => m (HashMap Text FilmRoll)
findImageSettings = do
  settingFiles <- findImageSettingFiles
  filmRollSettings <-
    liftIO $
      traverse
        ( \path ->
            (,) (Text.pack (makeRelative "./" (takeDirectory path)))
              <$> Aeson.eitherDecodeFileStrict path
        )
        settingFiles
  pure $
    foldr
      ( \(path, eitherSettings) acc ->
          either
            (const acc)
            (\settings -> HashMap.insert path settings acc)
            eitherSettings
      )
      mempty
      filmRollSettings

findImageSettingFiles :: MonadIO m => m [FilePath]
findImageSettingFiles =
  liftIO $
    (<>)
      <$> Glob.glob "./**/[Roll]*/image-settings.json"
      <*> Glob.glob "./image-settings.json"

diffedPreviewSettings :: FilePath -> FilePath -> IO FilmRoll
diffedPreviewSettings a b = do
  xs <- Aeson.decodeFileStrict $ a </> "image-settings.json"
  ys <- Aeson.decodeFileStrict $ b </> "image-settings.json"
  maybe (fail "Something went wrong decoding the settings files") pure $
    difference <$> xs <*> ys

insertPreviewSettings :: FilePath -> ImageSettings -> IO ()
insertPreviewSettings ps settings = do
  previewSettings <- Aeson.decodeFileStrict ps
  maybe
    (fail "Something went wrong decoding the settings file")
    (Aeson.encodeFile ps . insert settings)
    previewSettings

ensureUniqueFilename :: MonadIO m => FilePath -> m FilePath
ensureUniqueFilename filepath = do
  current <- liftIO $ Glob.glob (dropExtension filepath <> "*")
  pure $
    if null current
      then filepath
      else
        let toNumbers = read @Int . reverse . takeWhile isDigit . reverse . dropExtension
            preExt = case sortOn Down $ toNumbers <$> filter (/= filepath) current of
              n : _ -> "-" <> show (n + 1)
              _ -> "-1"
         in mconcat [dropExtension filepath, preExt, takeExtension filepath]
