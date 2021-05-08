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
      (logError @"stdout" "preview" . tshow)
      pure
    & runLabelled @"stdout"
    & runLogStdout logger

-- LOOKUP

withLookup ::
  ( HasLabelled "stdout" Log sig m,
    Has PostgreSQL sig m,
    Has (Lift IO) sig m,
    Has (Throw PostgreSQL.Error) sig m
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
  image <-
    sendIO . Image.fromDiskPreProcess Nothing imageSettings.crop $
      Path.append dir imageSettings.filename
  sendIO $ createDirectoryIfMissing False (Path.toFilePath dir </> "highres")
  outputWithCount <-
    sendIO . Util.ensureUniqueFilename $
      Path.toFilePath dir </> "highres" </> Path.toFilePath imageSettings.filename
  logInfo @"stdout" "generate highres" $
    "Generating highres version: " <> Text.pack outputWithCount
  either
    (logInfo @"stdout" "generate highres" . tshow)
    (sendIO . HIP.writeImage outputWithCount . Image.applySettings imageSettings)
    image
