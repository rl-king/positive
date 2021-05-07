{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Preview where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Effect.Labelled
import Control.Effect.Lift
import Control.Effect.Throw
import qualified Graphics.Image as HIP
import Positive.Data.Id
import Positive.Data.ImageSettings as ImageSettings
import qualified Positive.Data.Path as Path
import qualified Positive.Database.Session as Session
import Positive.Effect.Log
import Positive.Effect.PostgreSQL (PostgreSQL)
import qualified Positive.Effect.PostgreSQL as PostgreSQL
import qualified Positive.Image as Image
import Positive.Prelude hiding (ByteString)
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
      logInfo @"stdout" "preview" "Found no outdated previews"
      loop lock
    outdatedPreviews@((dir, imageSettings) : rest) -> do
      -- FIXME: catch exc
      logInfo @"stdout" "preview" $
        "Found " <> tshow (length outdatedPreviews) <> " outdated previews"
      _ <- generatePreview (dir, imageSettings)
      logInfo @"sse" "preview" $ Path.unpack imageSettings.filename
      _ <- PostgreSQL.runSession $ Session.updatePreviewTimestamp imageSettings.id
      unless (null rest) $ sendIO (void (tryPutMVar lock key))
      logInfo @"stdout" "preview" "Generated preview"

      loop lock

addCount :: (Text -> IO ()) -> [a] -> Text -> IO ()
addCount logInfo xs t =
  logInfo (t <> " | " <> tshow (length xs) <> " left in queue")

-- WRITE

generatePreview ::
  (HasLabelled "stdout" Log sig m, Has (Lift IO) sig m) =>
  (Path.Directory, ImageSettings) ->
  m (Either Path.Directory ImageSettingsId)
generatePreview (Path.toFilePath -> dir, imageSettings) = do
  let input =
        dir </> Path.toFilePath imageSettings.filename
      output =
        dir
          </> "previews"
          </> replaceExtension (Path.toFilePath imageSettings.filename) ".jpg"
  eitherImage <- sendIO $ Image.fromDiskPreProcess (Just 750) imageSettings.crop input
  case eitherImage of
    Left _ -> pure $ Left "Error generating preview"
    Right image -> do
      -- FIXME: catch exc
      sendIO . HIP.writeImage output $ Image.applySettings imageSettings image
      pure $ Right imageSettings.id

--     logInfo $ Text.unwords ["Error generating preview", Text.pack output]
--     logInfo $
-- Text.unwords
--   [ "Took",
--     tshow (Time.diffUTCTime done start),
--     "generating preview",
--     Text.pack output
--   ]
