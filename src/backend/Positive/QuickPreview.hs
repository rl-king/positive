{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.QuickPreview where

import Control.Carrier.Error.Church as Error.Church
import Control.Effect.Labelled hiding (Handler)
import Control.Effect.Lift
import qualified Data.Text as Text
import qualified Graphics.Image as HIP
import qualified Hasql.Pool as Hasql
import qualified Positive.Data.Id as Id
import Positive.Data.ImageSettings (ImageSettings)
import qualified Positive.Data.ImageSettings as ImageSettings
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

run :: TimedFastLogger -> FilePath -> IO ()
run logger filepath =
  withLookup filepath
    & runLabelled @"stdout"
    & runLogStdout logger


-- LOOKUP

withLookup ::
  ( HasLabelled "stdout" Log sig m
  , Has (Lift IO) sig m
  ) =>
  FilePath ->
  m ()
withLookup filepath = do
  (dir, filename) <-
    bimap Path.fromFilePath Path.fromFilePath
      . splitFileName
      <$> sendIO (makeAbsolute filepath)
  generate dir filename


-- GENERATE

generate ::
  (HasLabelled "stdout" Log sig m, Has (Lift IO) sig m) =>
  Path.Directory ->
  Path.Filename ->
  m ()
generate dir filename = do
  let inputPath = Path.append dir filename
      outputDir = Path.appendDir dir (Path.fromFilePath "quick-preview")
      outputPath = Path.append outputDir filename
  sendIO $ createDirectoryIfMissing True (Path.toFilePath outputDir)
  logTrace @"stdout" "generate quick-preview" $
    "generating quick-preview version: " <> Text.pack outputPath
  resizedImage <-
    sendIO (Image.fromDiskPreProcess Nothing ImageSettings.quickCrop inputPath)
  either
    (logTrace @"stdout" "generate quick-preview" . tshow)
    ( sendIO
        . HIP.writeImage outputPath
        . Image.applySettings (ImageSettings.emptyImageSettings (Id.pack 0) filename)
    )
    resizedImage
  logTrace @"stdout" "generate quick-preview" $
    "successfully generated quick-preview version: " <> Text.pack outputPath
