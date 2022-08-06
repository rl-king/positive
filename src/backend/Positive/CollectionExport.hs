{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.CollectionExport where

import Control.Carrier.Error.Church as Error.Church
import Control.Effect.Labelled hiding (Handler)
import Control.Effect.Lift
import qualified Data.Text as Text
import qualified Graphics.Image as HIP
import qualified Hasql.Pool as Hasql
import Positive.Data.Collection (Collection)
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


-- GENERATE

generate ::
  (HasLabelled "stdout" Log sig m, Has (Lift IO) sig m) =>
  Collection ->
  Path.Directory ->
  ImageSettings ->
  m ()
generate collection dir imageSettings = do
  homeDir <- sendIO getHomeDirectory
  let outputDir =
        homeDir
          </> "Pictures/PositiveCollections"
          </> Text.unpack collection.title
  sendIO $ createDirectoryIfMissing True outputDir
  let outputNameWithoutHash =
        outputDir </> Path.toFilePath imageSettings.filename
  logTrace @"stdout" "generate collection export" $
    "generating collection export version: " <> Text.pack outputNameWithoutHash
  image <-
    sendIO . Image.fromDiskPreProcess Nothing imageSettings.crop $
      Path.append dir imageSettings.filename
  either
    (logTrace @"stdout" "generate collection export" . tshow)
    (sendIO . HIP.writeImage outputNameWithoutHash . Image.applySettings imageSettings)
    image
  logTrace @"stdout" "generate collection export" $
    "successfully generated collection export version: " <> Text.pack outputNameWithoutHash
