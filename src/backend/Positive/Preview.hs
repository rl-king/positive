{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Preview where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified Graphics.Image as HIP
import qualified Hasql.Pool as Hasql
import qualified Hasql.Session as Hasql
import Network.Wai.EventSource
import qualified Positive.Data.Filename as Filename
import Positive.Data.FilmRoll (FilmRoll)
import qualified Positive.Data.FilmRoll as FilmRoll
import Positive.Data.Id
import Positive.Data.ImageSettings as ImageSettings
import qualified Positive.Database.Session as Session
import qualified Positive.Image as Image
import qualified Positive.Image.Util as Util
import Positive.Prelude hiding (ByteString)
import System.FilePath.Posix

-- LOOP

loop :: Hasql.Pool -> MVar () -> Chan ServerEvent -> (Text -> IO ()) -> IO ()
loop pool lock eventChan log = do
  key <- takeMVar lock
  eitherPreviews <- Hasql.use pool $ Session.selectOutdatedPreviews
  case eitherPreviews of
    Left err -> do
      log (tshow err)
    Right [] -> do
      log $ "Found no outdated previews"
      loop pool lock eventChan log
    Right outdatedPreviews@((dir, imageSettings) : rest) -> do
      -- FIXME: catch exc
      log $ "Found " <> tshow (length outdatedPreviews) <> " outdated previews"
      _ <- generatePreview (dir, imageSettings)
      writeChan eventChan $
        ServerEvent
          (Just "preview")
          Nothing
          [Builder.byteString $ Filename.toByteString imageSettings.filename]
      _ <-
        Hasql.use pool $
          Session.updatePreviewTimestamp imageSettings.id
      unless (null rest) $ void (tryPutMVar lock key)
      loop pool lock eventChan log

addCount :: (Text -> IO ()) -> [a] -> Text -> IO ()
addCount log xs t =
  log (t <> " | " <> tshow (length xs) <> " left in queue")

-- WRITE

generatePreview :: (Text, ImageSettings) -> IO (Either Text ImageSettingsId)
generatePreview (Text.unpack -> dir, imageSettings) = do
  let input =
        dir </> Filename.toFilePath imageSettings.filename
      output =
        dir
          </> "previews"
          </> replaceExtension (Filename.toFilePath imageSettings.filename) ".jpg"
  eitherImage <- Image.fromDiskPreProcess (Just 750) imageSettings.crop input
  case eitherImage of
    Left _ -> pure $ Left "Error generating preview"
    Right image -> do
      -- FIXME: catch exc
      HIP.writeImage output $ Image.applySettings imageSettings image
      pure $ Right imageSettings.id

--     log $ Text.unwords ["Error generating preview", Text.pack output]
--     log $
-- Text.unwords
--   [ "Took",
--     tshow (Time.diffUTCTime done start),
--     "generating preview",
--     Text.pack output
--   ]
