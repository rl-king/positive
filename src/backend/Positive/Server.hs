{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Server
  ( run,
  )
where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Massiv.Array as Massiv
import qualified Data.OrdPSQ as OrdPSQ
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified Graphics.Image as HIP
import Network.Wai.EventSource
import Network.Wai.Handler.Warp hiding (run)
import Positive.Api
import Positive.Flags (Flags (..))
import qualified Positive.Image as Image
import Positive.ImageSettings (FilmRollSettings, ImageSettings (..))
import qualified Positive.ImageSettings as ImageSettings
import qualified Positive.Log as Log
import Positive.Prelude hiding (ByteString)
import qualified Positive.Preview as Preview
import qualified Positive.Static as Static
import Servant
import Servant.Server.Generic
import System.Directory
import System.FilePath.Posix ((</>), isPathSeparator)

-- POSITIVE

type PositiveT m =
  ReaderT Env m

data Env = Env
  { imageMVar :: !(MVar (OrdPSQ Text UTCTime Image.MonochromeImage)),
    previewMVar :: !(MVar [(FilePath, ImageSettings)]),
    eventChan :: !(Chan ServerEvent),
    isDev :: !Bool,
    logger :: !Log.TimedFastLogger
  }

-- SERVER

run :: Log.TimedFastLogger -> Flags -> IO ()
run logger_ flags =
  let settings =
        setPort 8080 $
          setBeforeMainLoop
            (Log.log logger_ ("listening on port " <> tshow @Int 8080))
            defaultSettings
   in do
        imageMVar_ <- newMVar OrdPSQ.empty
        previewMVar_ <- newEmptyMVar
        eventChan_ <- newChan
        let env = Env imageMVar_ previewMVar_ eventChan_ flags.isDev logger_
        Preview.loop previewMVar_ eventChan_ (Log.log logger_)
        runSettings settings (genericServeT (`runReaderT` env) (handlers flags.isDev eventChan_))

-- HANDLERS

handlers :: Bool -> Chan ServerEvent -> Api (AsServerT (PositiveT Handler))
handlers isDev_ chan =
  Api
    { aImageApi =
        genericServerT
          ImageApi
            { iaImage = handleImage,
              iaEvents = pure $ eventSourceAppChan chan,
              iaRaw = Static.serve isDev_
            },
      aSettingsApi =
        genericServerT
          SettingsApi
            { saSaveSettings = handleSaveSettings,
              saGetSettings = handleGetSettings,
              saGetSettingsHistogram = handleGetSettingsHistogram,
              saGenerateHighRes = handleGenerateHighRes,
              saGenerateWallpaper = handleGenerateWallpaper
            }
    }

handleImage :: Text -> ImageSettings -> PositiveT Handler ByteString
handleImage dir settings = do
  env <- ask
  (image, putMVarBack) <-
    getImage $
      Text.pack (Text.unpack dir </> Text.unpack settings.iFilename)
  if env.isDev
    then do
      processed <- timed "Apply" $ Image.processImage settings image
      encoded <- timed "Encode" =<< Image.encode "_.png" processed
      liftIO putMVarBack
      pure encoded
    else do
      log $ "Apply settings and encode: " <> settings.iFilename
      encoded <- Image.encode "_.png" $ Image.processImage settings image
      liftIO putMVarBack
      pure encoded

handleSaveSettings :: Text -> FilmRollSettings -> PositiveT Handler FilmRollSettings
handleSaveSettings dir newSettings = do
  liftIO $ Aeson.encodeFile (Text.unpack dir </> "image-settings.json") newSettings
  logDebug "Wrote settings"
  env <- ask
  missing <- liftIO Preview.findMissingPreviews
  void . liftIO $ tryPutMVar env.previewMVar missing
  logDebug $ "Updating " <> tshow (length missing) <> " preview(s)"
  pure newSettings

-- GENERATE PREVIEWS

handleGenerateHighRes :: Text -> ImageSettings -> PositiveT Handler NoContent
handleGenerateHighRes dir settings = do
  liftIO $ createDirectoryIfMissing False (Text.unpack dir </> "highres")
  let input = Text.unpack dir </> Text.unpack settings.iFilename
      output = Text.unpack dir </> "highres" </> Text.unpack settings.iFilename
  log $ "Generating highres version of: " <> Text.pack input
  maybeImage <- liftIO $ Image.readImageFromDisk input
  case maybeImage of
    Left _ ->
      log "Image read error" >> throwError err404
    Right image -> do
      outputWithCount <- liftIO $ ImageSettings.pickFilename output
      liftIO . HIP.writeImage outputWithCount $ Image.processImage settings image
      log $ "Wrote highres version of: " <> Text.pack input
      pure NoContent

handleGenerateWallpaper :: Text -> ImageSettings -> PositiveT Handler NoContent
handleGenerateWallpaper dir settings = do
  let input = Text.unpack dir </> Text.unpack settings.iFilename
      output =
        "/Users/king/Documents/wallpapers/positive"
          </> filter (not . isPathSeparator) (Text.unpack dir)
          <> " | "
          <> Text.unpack settings.iFilename
  log $ "Generating wallpaper version of: " <> Text.pack input
  maybeImage <- liftIO $ Image.readImageFromDisk input
  case maybeImage of
    Left _ ->
      log "Image read error" >> throwError err404
    Right image -> do
      liftIO . HIP.writeImage output . Image.resizeImage 2560 $ Image.processImage settings image
      log $ "Wrote wallpaper version of: " <> Text.pack input
      pure NoContent

-- HISTOGRAM

handleGetSettingsHistogram :: Text -> ImageSettings -> PositiveT Handler [Int]
handleGetSettingsHistogram dir settings =
  let bins = HashMap.fromList [(x, 0) :: (Word8, Int) | x <- [0 .. 255]]
      adjust acc (HIP.PixelY x) = HashMap.adjust (+ 1) (floor (255 * x)) acc
   in do
        (image, putMVarBack) <-
          getImage $
            Text.pack (Text.unpack dir </> Text.unpack settings.iFilename)
        liftIO putMVarBack
        logDebug $ "Creating histogram for: " <> settings.iFilename
        fmap (fmap snd . sortOn fst . HashMap.toList)
          . Massiv.foldlP adjust bins (HashMap.unionWith (+)) bins
          . HIP.unImage
          $ Image.processImage settings image

-- LIST DIRECTORIES

handleGetSettings :: PositiveT Handler [(Text, FilmRollSettings)]
handleGetSettings =
  HashMap.toList <$> liftIO ImageSettings.findImageSettings

-- IMAGE

getImage :: Text -> PositiveT Handler (Image.MonochromeImage, IO ())
getImage path = do
  env <- ask
  now <- liftIO Time.getCurrentTime
  cache <- liftIO $ takeMVar env.imageMVar
  logDebug $ "Cached images: " <> tshow (OrdPSQ.size cache)
  case OrdPSQ.lookup path cache of
    Just (_, loadedImage) -> do
      logDebug "From cache"
      pure (loadedImage, putMVar env.imageMVar $ insertAndTrim path now loadedImage cache)
    Nothing -> do
      logDebug "From disk"
      maybeImage <- liftIO $ Image.readImageFromDisk (Text.unpack path)
      case Image.resizeImage 1440 <$> maybeImage of
        Left err -> log ("Image read error: " <> tshow err) >> throwError err404
        Right image ->
          pure (image, putMVar env.imageMVar $ insertAndTrim path now image cache)

insertAndTrim :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
insertAndTrim k v p psq =
  let q = OrdPSQ.insert k v p psq
   in if OrdPSQ.size q > 40 then OrdPSQ.deleteMin q else q

-- LOG

log :: MonadIO m => Text -> PositiveT m ()
log msg = do
  env <- ask
  liftIO . writeChan env.eventChan $ ServerEvent (Just "log") Nothing [Builder.byteString $ encodeUtf8 msg]
  liftIO . env.logger $ Log.format "info" msg

logDebug :: MonadIO m => Text -> PositiveT m ()
logDebug msg = do
  env <- ask
  when env.isDev $ do
    liftIO . writeChan env.eventChan $ ServerEvent (Just "log") Nothing [Builder.byteString $ encodeUtf8 msg]
    liftIO . env.logger $ Log.format "debug" msg

-- PROFILE

timed :: Text -> a -> PositiveT Handler a
timed name action = do
  logDebug $ name <> " - started"
  start <- liftIO Time.getCurrentTime
  a <- liftIO $ evaluate action
  done <- liftIO Time.getCurrentTime
  logDebug $ name <> " - processed in: " <> tshow (Time.diffUTCTime done start)
  pure a
