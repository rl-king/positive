{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Metadata where

import Control.Concurrent.MVar
import Control.Effect.Labelled
import Control.Effect.Lift
import Control.Effect.Throw
import qualified Data.Massiv.Array as Massiv
import qualified Data.Massiv.Array.Manifest.Vector as Massiv
import qualified Data.Massiv.Array.Mutable as Massiv.Mutable
import qualified Graphics.Image as HIP
import qualified Positive.Data.Id as Id
import Positive.Data.ImageSettings
import Positive.Data.Metadata
import qualified Positive.Data.Path as Path
import qualified Positive.Database.Session as Session
import Positive.Effect.Log
import Positive.Effect.PostgreSQL (PostgreSQL)
import qualified Positive.Effect.PostgreSQL as PostgreSQL
import qualified Positive.Image as Image
import Positive.Prelude hiding (ByteString)
import Positive.Timed
import System.FilePath.Posix

-- LOOP

loop ::
  ( HasLabelled "stdout" Log sig m,
    HasLabelled "sse" Log sig m,
    Has PostgreSQL sig m,
    Has (Lift IO) sig m,
    Has (Throw PostgreSQL.Error) sig m
  ) =>
  MVar () ->
  m ()
loop lock = do
  key <- sendIO $ takeMVar lock
  previews <- PostgreSQL.runSession Session.selectOutdatedPreviews
  case previews of
    [] -> do
      logInfo @"stdout" "metadata" "found no outdated previews"
      loop lock
    outdatedPreviews@((dir, imageSettings) : rest) -> do
      logInfo @"stdout" "metadata" $
        "found " <> tshow (length outdatedPreviews) <> " outdated previews"
      timedM "metadata" $ updateMetadata dir imageSettings
      unless (null rest) $ sendIO (void (tryPutMVar lock key))
      loop lock

-- WRITE

updateMetadata ::
  ( HasLabelled "stdout" Log sig m,
    HasLabelled "sse" Log sig m,
    Has PostgreSQL sig m,
    Has (Lift IO) sig m,
    Has (Throw PostgreSQL.Error) sig m
  ) =>
  Path.Directory ->
  ImageSettings ->
  m ()
updateMetadata (Path.toFilePath -> dir) imageSettings =
  let input = dir </> Path.toFilePath imageSettings.filename
      output =
        dir </> "previews"
          </> replaceExtension
            (Path.toFilePath imageSettings.filename)
            ".jpg"
   in do
        resizedImage <-
          sendIO (Image.fromDiskPreProcess (Just 750) imageSettings.crop input)
        case resizedImage of
          Left _ ->
            logError @"stdout" "metadata" "unable to load image"
          Right image -> do
            sendIO . HIP.writeImage output $
              Image.applySettings imageSettings image
            logInfo @"sse" "preview" . tshow $ Id.unpack imageSettings.id
            PostgreSQL.runSession . Session.updateMetadata $
              MetadataBase
                { id = Nothing,
                  imageId = imageSettings.id,
                  preview_updated = Nothing,
                  histogram =
                    Massiv.toVector . generateHistogram $ HIP.unImage image
                }
            logInfo @"stdout" "metadata" "generated preview"

generateHistogram ::
  ( HIP.Source r ix (HIP.Pixel HIP.Y e),
    HIP.Elevator e
  ) =>
  HIP.Array r ix (HIP.Pixel HIP.Y e) ->
  HIP.Array Massiv.P HIP.Ix1 Int
generateHistogram arr =
  Massiv.Mutable.createArrayST_ @Massiv.P @_ @Int
    (Massiv.Sz1 (1 + fromIntegral (maxBound :: Word8)))
    $ \marr ->
      Massiv.forM_ arr $
        \(HIP.PixelY p) ->
          Massiv.modify marr (pure . (+) 1) (fromIntegral (HIP.toWord8 p))
