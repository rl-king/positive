{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Preview where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified Graphics.Image as HIP
import Network.Wai.EventSource
import qualified Positive.Data.Filename as Filename
import Positive.Data.FilmRoll (FilmRoll)
import qualified Positive.Data.FilmRoll as FilmRoll
import Positive.Data.ImageSettings as ImageSettings
import qualified Positive.Image as Image
import qualified Positive.Image.Util as Util
import Positive.Prelude hiding (ByteString)
import System.FilePath.Posix

-- PREVIEW

run :: (Text -> IO ()) -> Bool -> IO ()
run log replace = do
  missing <- findMissingPreviews replace
  log $ Text.unwords ["Found", tshow $ length missing, "outdated preview(s)"]
  for_ (zip missing [1 :: Int ..]) $ \(x, index) ->
    generatePreview (addCount log (drop index missing)) x

-- LOOP

loop ::
  MVar [(FilePath, ImageSettings)] ->
  MVar (OrdPSQ Text UTCTime (ImageSettings.ImageCrop, Image.Monochrome)) ->
  Chan ServerEvent ->
  (Text -> IO ()) ->
  IO ()
loop queueMVar cacheMVar eventChan log =
  void . forkIO . forever $
    -- block while some other process uses cacheMVar
    takeMVar queueMVar <* readMVar cacheMVar >>= \case
      [] -> log "Finished generating previews"
      x@(_, is) : xs -> do
        generatePreview (addCount log xs) x
        writeChan eventChan $
          ServerEvent
            (Just "preview")
            Nothing
            [Builder.byteString $ Filename.toByteString is.filename]
        void (tryPutMVar queueMVar xs)

addCount :: (Text -> IO ()) -> [a] -> Text -> IO ()
addCount log xs t =
  log (t <> " | " <> tshow (length xs) <> " left in queue")

-- FIND

findMissingPreviews :: MonadIO m => Bool -> m [(FilePath, ImageSettings)]
findMissingPreviews replace =
  error "todo"

-- let toSettings dir =
--       if replace
--         then
--           maybe (fail "Something went wrong reading the settings file") pure
--             =<< Aeson.decodeFileStrict (dir </> "image-settings.json")
--         else Util.diffedPreviewSettings dir (dir </> "previews")
--  in liftIO $
--       fmap (sortOn fst . concat)
--         . mapM (\dir -> prependDir dir <$> toSettings dir)
--         . fmap dropFileName
--         =<< Util.findImageSettingFiles

prependDir :: FilePath -> FilmRoll -> [(FilePath, ImageSettings)]
prependDir dir settings =
  (\x -> (dir </> Filename.toFilePath x.filename, x)) <$> FilmRoll.toList settings

-- WRITE

generatePreview :: (Text -> IO ()) -> (FilePath, ImageSettings) -> IO ()
generatePreview log (input, is) = do
  error "todo"

-- let dir = dropFileName input
--     output = dir </> "previews" </> replaceExtension (Filename.toFilePath is.filename) ".jpg"
-- start <- Time.getCurrentTime
-- maybeImage <- Image.fromDiskPreProcess (Just 750) is.crop input
-- case maybeImage of
--   Left _ ->
--     log $ Text.unwords ["Error generating preview", Text.pack output]
--   Right image -> do
--     HIP.writeImage output $ Image.applySettings is image
--     Util.insertPreviewSettings (dir </> "previews" </> "image-settings.json") is
--     done <- liftIO Time.getCurrentTime
--     log $
--       Text.unwords
--         [ "Took",
--           tshow (Time.diffUTCTime done start),
--           "generating preview",
--           Text.pack output
--         ]
