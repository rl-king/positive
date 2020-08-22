{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Positive.Server where

import Control.Algebra
import Control.Carrier.Error.Church as Error.Church
import Control.Carrier.Error.Either as Error.Either
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified Data.Vector.Unboxed as Vector
import qualified Graphics.Image as HIP
import Network.Wai.Handler.Warp
import Positive.Effect.FilmRoll as FilmRoll
import Positive.Effect.Log
import Positive.Flags
import Positive.Image
import Positive.ImageSettings as ImageSettings
import Positive.Prelude hiding (ByteString)
import qualified Positive.Static as Static
import Servant hiding (throwError)
import Servant.API.Generic
import Servant.Server.Generic
import System.Directory
import qualified System.FilePath.Posix as Path
import System.FilePath.Posix ((</>))
import System.Log.FastLogger (TimedFastLogger)

-- POSITIVE

data Env = Env
  { eImageMVar :: !(MVar (Text, MonochromeImage HIP.VU)),
    ePreviewMVar :: !(MVar FilmRollSettings),
    eDir :: !PathSegment
  }

-- API

data Api route = Api
  { aSettingsApi :: route :- ToServantApi SettingsApi,
    aImageApi :: route :- ToServantApi ImageApi
  }
  deriving (Generic)

data ImageApi route = ImageApi
  { iaImage ::
      route :- "image"
        :> QueryParam' '[Required, Strict] "preview-width" Int
        :> QueryParam' '[Required, Strict] "image-settings" ImageSettings
        :> Get '[Image] ByteString,
    iaRaw :: route :- Raw
  }
  deriving (Generic)

data SettingsApi route = SettingsApi
  { saSaveSettings ::
      route :- "image" :> "settings"
        :> ReqBody '[JSON] [ImageSettings]
        :> Post '[JSON] [ImageSettings],
    saGetSettings ::
      route :- "image" :> "settings"
        :> Get '[JSON] ([ImageSettings], PathSegment),
    saGetSettingsHistogram ::
      route :- "image" :> "settings" :> "histogram"
        :> QueryParam' '[Required, Strict] "preview-width" Int
        :> ReqBody '[JSON] ImageSettings
        :> Post '[JSON] [Int],
    saGenerateHighRes ::
      route :- "image" :> "settings" :> "highres"
        :> ReqBody '[JSON] ImageSettings
        :> PostNoContent '[JSON] NoContent
  }
  deriving (Generic)

-- SERVER

server :: TimedFastLogger -> Flags -> IO ()
server logger Flags {fDir, fIsDev} =
  let settings =
        setPort 8080 $
          setBeforeMainLoop
            (log_ logger ("listening on port " <> tshow @Int 8080))
            defaultSettings
      runEffects imageMVar previewMVar =
        runReader (Env imageMVar previewMVar fDir)
          >>> runFilmRoll fDir
          >>> Error.Church.runError (\err -> log (Text.pack err) >> throwError err500) pure
          >>> Error.Either.runError
          >>> runLog logger
          >>> runM
          >>> ExceptT
          >>> Servant.Handler
   in do
        imageMVar <- newMVar ("", HIP.fromLists [[HIP.PixelY 1]])
        previewMVar <- newEmptyMVar
        previewWorker logger previewMVar fDir
        runSettings settings $
          genericServeT (runEffects imageMVar previewMVar) (handlers fIsDev)

-- HANDLERS

handlers ::
  ( Has Log sig m,
    Has (Reader Env) sig m,
    Has FilmRoll sig m,
    Has (Lift IO) sig m
  ) =>
  Bool ->
  Api (AsServerT m)
handlers isDev =
  Api
    { aImageApi =
        genericServerT
          ImageApi
            { iaImage = handleImage,
              iaRaw = Static.serve isDev
            },
      aSettingsApi =
        genericServerT
          SettingsApi
            { saSaveSettings = handleSaveSettings,
              saGetSettings = handleGetSettings,
              saGetSettingsHistogram = handleGetSettingsHistogram,
              saGenerateHighRes = handleGenerateHighRes
            }
    }

handleImage ::
  ( Has (Lift IO) sig m,
    Has (Reader Env) sig m,
    Has Log sig m
  ) =>
  Int ->
  ImageSettings ->
  m ByteString
handleImage previewWidth imageSettings = do
  dir <- workingDirectory
  (image, putMVarBack) <-
    getImage previewWidth $
      Text.pack (dir </> Text.unpack (iFilename imageSettings))
  processed <- timed "Apply settings" $ processImage imageSettings image
  encoded <- timed "Encode PNG" $ HIP.encode HIP.PNG [] $ HIP.exchange HIP.VS processed
  sendIO putMVarBack
  pure encoded

handleSaveSettings ::
  ( Has (Lift IO) sig m,
    Has (Reader Env) sig m,
    Has FilmRoll sig m,
    Has Log sig m
  ) =>
  [ImageSettings] ->
  m [ImageSettings]
handleSaveSettings settings = do
  Env {ePreviewMVar} <- ask
  let newSettings = ImageSettings.fromList settings
  FilmRoll.writeSettings newSettings
  log "Updated settings"
  sendIO $ pSwapMVar ePreviewMVar newSettings
  pure $ ImageSettings.toList newSettings

handleGetSettings ::
  ( Has (Reader Env) sig m,
    Has FilmRoll sig m
  ) =>
  m ([ImageSettings], PathSegment)
handleGetSettings =
  (\a b -> (ImageSettings.toList a, eDir b)) <$> FilmRoll.readSettings <*> ask

handleGenerateHighRes ::
  ( Has (Lift IO) sig m,
    Has (Reader Env) sig m,
    Has Log sig m
  ) =>
  ImageSettings ->
  m NoContent
handleGenerateHighRes settings = do
  dir <- workingDirectory
  sendIO $ createDirectoryIfMissing False (dir </> "highres")
  let input = dir </> Text.unpack (iFilename settings)
      output = dir </> "highres" </> Text.unpack (iFilename settings)
  log $ "Generating highres version of: " <> Text.pack input
  image <-
    sendIO $
      HIP.encode HIP.PNG [] . HIP.exchange HIP.VS . processImage settings
        <$> readImageFromDisk input
  sendIO $ ByteString.writeFile output image
  log $ "Wrote highres version of: " <> Text.pack input
  pure NoContent

-- HISTOGRAM

handleGetSettingsHistogram ::
  ( Has (Lift IO) sig m,
    Has (Reader Env) sig m,
    Has Log sig m
  ) =>
  Int ->
  ImageSettings ->
  m [Int]
handleGetSettingsHistogram previewWidth imageSettings = do
  dir <- workingDirectory
  (image, putMVarBack) <-
    getImage previewWidth $
      Text.pack (dir </> Text.unpack (iFilename imageSettings))
  sendIO putMVarBack
  log $ "Generating histogram fro: " <> iFilename imageSettings
  pure . Vector.toList . HIP.hBins . head . HIP.getHistograms $
    processImage imageSettings image

-- IMAGE

getImage ::
  ( Has (Lift IO) sig m,
    Has (Reader Env) sig m,
    Has Log sig m
  ) =>
  Int ->
  Text ->
  m (MonochromeImage HIP.VU, IO ())
getImage previewWidth path = do
  Env {eImageMVar} <- ask
  currentlyLoaded@(loadedPath, loadedImage) <- sendIO $ takeMVar eImageMVar
  log $ "MVar: " <> tshow currentlyLoaded
  if path == loadedPath && HIP.cols loadedImage == previewWidth
    then do
      log "Loaded image"
      pure (loadedImage, putMVar eImageMVar currentlyLoaded)
    else do
      log "Reading image"
      image <- sendIO $ resizeImage previewWidth <$> readImageFromDisk (Text.unpack path)
      log "Read image"
      pure (image, putMVar eImageMVar (path, image))

-- PREVIEW LOOP

previewWorker :: TimedFastLogger -> MVar FilmRollSettings -> PathSegment -> IO ()
previewWorker logger previewMVar dir =
  void . forkIO . forever $ do
    queuedSettings <- takeMVar previewMVar
    result <-
      runM . Error.Either.runError . runFilmRoll dir $ do
        currentSettings <- FilmRoll.readPreviewSettings
        writePreviewSettings queuedSettings
        pure $ difference queuedSettings currentSettings
    case result of
      Left err ->
        log_ logger (Text.pack err) >> threadDelay 10000000
      Right newSettings -> do
        for_ (toList newSettings) $
          \settings -> do
            let input = dir <> PathSegment (iFilename settings)
                output =
                  dir
                    <> PathSegment "previews"
                    <> PathSegment (Text.pack (Path.replaceExtension (Text.unpack (iFilename settings)) ".jpg"))
            log_ logger $ "Generating preview for: " <> tshow input
            ByteString.writeFile (segmentToString output)
              =<< HIP.encode HIP.JPG [] . HIP.exchange HIP.VS . processImage settings . resizeImage 750
              <$> readImageFromDisk (segmentToString input)
        threadDelay 10000000

-- PROFILE

timed :: (Has (Lift IO) sig m, Has Log sig m) => Text -> a -> m a
timed name action = do
  log $ name <> " - started"
  start <- sendIO Time.getCurrentTime
  a <- sendIO $ evaluate action
  done <- sendIO Time.getCurrentTime
  log $ name <> " - processed in: " <> tshow (Time.diffUTCTime done start)
  pure a

-- HELPERS

workingDirectory :: Has (Reader Env) sig m => m FilePath
workingDirectory =
  asks (segmentToString . eDir)

pSwapMVar :: MVar a -> a -> IO ()
pSwapMVar mvar a = do
  isEmpty <- isEmptyMVar mvar
  print isEmpty
  if isEmpty
    then putMVar mvar a
    else void $ swapMVar mvar a
