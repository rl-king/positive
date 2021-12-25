{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Image.Util where

import Control.Effect.Labelled
import Control.Effect.Lift
import qualified Data.Text as Text
import qualified Graphics.Image as HIP
import Positive.Data.Id
import Positive.Data.ImageSettings
import Positive.Effect.Log
import Positive.Image as Image
import Positive.Prelude
import qualified System.Directory as Directory
import qualified System.FilePath.Glob as Glob
import System.FilePath.Posix as Path


-- FS

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


writeToCache ::
  ( Has (Lift IO) sig m
  , HasLabelled "stdout" Log sig m
  ) =>
  ImageSettingsId ->
  ImageCrop ->
  Image.Monochrome ->
  m ()
writeToCache imageSettingsId crop image = do
  cachePath <- toCacheFilePath imageSettingsId crop
  exists <- sendIO $ Directory.doesFileExist cachePath
  unless exists $ do
    logTraceShow @"stdout" "writing image to cache" imageSettingsId
    sendIO $ HIP.writeImage cachePath image


readImageFromWithCache ::
  ( Has (Lift IO) sig m
  , HasLabelled "stdout" Log sig m
  ) =>
  ImageSettingsId ->
  ImageCrop ->
  Text ->
  m (Either SomeException Image.Monochrome)
readImageFromWithCache imageSettingsId crop path = do
  cachePath <- toCacheFilePath imageSettingsId crop
  exists <- sendIO $ Directory.doesFileExist cachePath
  if exists
    then do
      logTraceShow @"stdout" "found cached image on disk" imageSettingsId
      sendIO $ Image.fromDisk cachePath
    else do
      logTraceShow @"stdout" "loading from source" imageSettingsId
      sendIO $
        Image.fromDiskPreProcess (Just 1440) crop (Text.unpack path)


toCacheFilePath ::
  Has (Lift IO) sig m =>
  ImageSettingsId ->
  ImageCrop ->
  m String
toCacheFilePath imageSettingsId crop = do
  homeDir <- sendIO Directory.getHomeDirectory
  pure $
    homeDir
      </> ".cache"
      </> "positive"
      </> show (unpack imageSettingsId) <> "-" <> show (hash crop) <> ".png"
