{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.SingleImage where

import Control.Carrier.Error.Church as Error.Church
import Control.Effect.Labelled hiding (Handler)
import Control.Effect.Lift
import qualified Data.Text as Text
import qualified Graphics.Image as HIP
import qualified Hasql.Pool as Hasql
import Positive.Data.ImageSettings (ImageSettings)
import qualified Positive.Data.Path as Path
import qualified Positive.Database.Session as Session
import Positive.Effect.Log
import Positive.Effect.PostgreSQL (PostgreSQL)
import qualified Positive.Effect.PostgreSQL as PostgreSQL
import qualified Positive.Image as Image
import qualified Positive.Image.Util as Util
import Positive.Prelude hiding (ByteString)
import System.Directory
import System.FilePath.Posix


-- SINGLE IMAGE

run :: TimedFastLogger -> Hasql.Pool -> FilePath -> IO ()
run logger pool filepath =
  withLookup filepath
    & PostgreSQL.runPostgreSQL pool
    & Error.Church.runError @PostgreSQL.Error
      (logError @"stdout" "generate-highres" . tshow)
      pure
    & runLabelled @"stdout"
    & runLogStdout logger


-- LOOKUP

withLookup ::
  ( HasLabelled "stdout" Log sig m
  , Has PostgreSQL sig m
  , Has (Lift IO) sig m
  , Has (Throw PostgreSQL.Error) sig m
  ) =>
  FilePath ->
  m ()
withLookup filepath = do
  (dir, filename) <-
    bimap Path.fromFilePath Path.fromFilePath
      . splitFileName
      <$> sendIO (makeAbsolute filepath)
  (dir, imageSettings) <-
    PostgreSQL.runSession $
      Session.selectImageSettingsByPath dir filename
  generate dir imageSettings


-- GENERATE

generate ::
  (HasLabelled "stdout" Log sig m, Has (Lift IO) sig m) =>
  Path.Directory ->
  ImageSettings ->
  m ()
generate dir imageSettings = do
  homeDir <- sendIO getHomeDirectory
  let outputDir =
        homeDir
          </> "Pictures/PositiveOutput"
          </> (last (splitDirectories (Path.toFilePath dir)))
  sendIO $ createDirectoryIfMissing True outputDir
  let outputNameWithHash =
        updateBaseName (\n -> n <> "-" <> show (hash imageSettings)) $
          outputDir </> Path.toFilePath imageSettings.filename
  exists <- sendIO $ doesFileExist outputNameWithHash
  if exists
    then
      logTrace @"stdout" "generate highres" $
        "skipping generation of highres version (exists): " <> Text.pack outputNameWithHash
    else do
      logTrace @"stdout" "generate highres" $
        "generating highres version: " <> Text.pack outputNameWithHash
      image <-
        sendIO . Image.fromDiskPreProcess Nothing imageSettings.crop $
          Path.append dir imageSettings.filename
      either
        (logTrace @"stdout" "generate highres" . tshow)
        (sendIO . HIP.writeImage outputNameWithHash . Image.applySettings imageSettings)
        image
      logTrace @"stdout" "generate highres" $
        "successfully generated highres version: " <> Text.pack outputNameWithHash


updateBaseName :: (FilePath -> FilePath) -> FilePath -> FilePath
updateBaseName f x =
  replaceBaseName x (f (takeBaseName x))
