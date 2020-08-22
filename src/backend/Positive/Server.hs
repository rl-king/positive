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
import GHC.Float (int2Double)
import qualified Graphics.Image as HIP
import qualified Network.HTTP.Media as Media
import Network.Wai.Handler.Warp
import Positive.Effect.FilmRoll as FilmRoll
import Positive.Effect.Log
import Positive.Flags
import Positive.Prelude hiding (ByteString)
import Positive.Settings as Settings
import qualified Positive.Static as Static
import Servant hiding (throwError)
import Servant.API.Generic
import Servant.Server.Generic
import System.Directory
import qualified System.FilePath.Posix as Path
import System.FilePath.Posix ((</>))
import System.Log.FastLogger (TimedFastLogger)

-- POSITIVE

type PositiveM sig m =
  ( Has Log sig m,
    Has (Reader Env) sig m,
    Has FilmRoll sig m,
    Has (Lift IO) sig m
  )

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
        :> PostNoContent '[JSON] NoContent,
    saGeneratePreviews ::
      route :- "image" :> "settings" :> "previews"
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
              saGenerateHighRes = handleGenerateHighRes,
              saGeneratePreviews = error "" -- handleGeneratePreviews
            }
    }

handleImage :: PositiveM sig m => Int -> ImageSettings -> m ByteString
handleImage previewWidth imageSettings = do
  dir <- workingDirectory
  (image, putMVarBack) <-
    getImage previewWidth $
      Text.pack (dir </> Text.unpack (iFilename imageSettings))
  processed <- timed "Apply settings" $ processImage imageSettings image
  encoded <- timed "Encode PNG" $ HIP.encode HIP.PNG [] $ HIP.exchange HIP.VS processed
  sendIO putMVarBack
  pure encoded

handleSaveSettings :: PositiveM sig m => [ImageSettings] -> m [ImageSettings]
handleSaveSettings settings = do
  Env {ePreviewMVar} <- ask
  let newSettings = Settings.fromList settings
  FilmRoll.writeSettings newSettings
  log "Updated settings"
  sendIO $ pSwapMVar ePreviewMVar newSettings
  pure $ Settings.toList newSettings

handleGetSettings :: PositiveM sig m => m ([ImageSettings], PathSegment)
handleGetSettings =
  (\a b -> (Settings.toList a, eDir b)) <$> FilmRoll.readSettings <*> ask

handleGenerateHighRes :: PositiveM sig m => ImageSettings -> m NoContent
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

handleGetSettingsHistogram :: PositiveM sig m => Int -> ImageSettings -> m [Int]
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

getImage :: PositiveM sig m => Int -> Text -> m (MonochromeImage HIP.VU, IO ())
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

-- IMAGE

type MonochromeImage arr =
  HIP.Image arr HIP.Y Double

type MonochromePixel =
  HIP.Pixel HIP.Y Double

readImageFromDisk :: String -> IO (MonochromeImage HIP.VU)
readImageFromDisk =
  HIP.readImageY HIP.VU

resizeImage :: Int -> MonochromeImage HIP.VU -> MonochromeImage HIP.VU
resizeImage targetWidth image =
  let mul = int2Double (HIP.rows image) / int2Double (HIP.cols image)
   in HIP.resize
        (HIP.Bicubic (-0.25))
        HIP.Edge
        (round (int2Double targetWidth * mul), targetWidth)
        image

processImage :: ImageSettings -> MonochromeImage HIP.VU -> MonochromeImage HIP.VU
processImage is image =
  let (h, w) = HIP.dims image
      cropOffset =
        ( floor $ int2Double h / 100 * icTop (iCrop is),
          floor $ int2Double w / 100 * icLeft (iCrop is)
        )
      cropWidth = floor $ int2Double (w - snd cropOffset) * (icWidth (iCrop is) / 100)
      cropHeight = floor $ int2Double cropWidth * mul
      mul = int2Double h / int2Double w
   in HIP.map
        ( zone 0.9 (iZone9 is)
            . zone 0.5 (iZone5 is)
            . zone 0.1 (iZone1 is)
            . gamma (iGamma is)
            . compress (iBlackpoint is) (iWhitepoint is)
            . invert
        )
        -- NOTE: 'normalize' is the only "automatic" correction and has a subtle effect
        -- at which it makes smaller images have more contrast due to different max and min
        -- values vs the original size
        . HIP.normalize
        . (if iRotate is == 0 then id else HIP.rotate (HIP.Bicubic (-0.25)) (HIP.Fill 0) (iRotate is))
        $ HIP.crop cropOffset (cropHeight, cropWidth) image

-- FILTERS

invert :: MonochromePixel -> MonochromePixel
invert =
  fmap (1 -)
{-# INLINE invert #-}

compress :: Double -> Double -> MonochromePixel -> MonochromePixel
compress s l =
  fmap (\p -> min 1 . max 0 $ (p - s) / (l - s))
{-# INLINE compress #-}

gamma :: Double -> MonochromePixel -> MonochromePixel
gamma x =
  fmap (** x)
{-# INLINE gamma #-}

zone :: Double -> Double -> MonochromePixel -> MonochromePixel
zone t i =
  let m v = (1 - abs (v - t)) ^ (2 :: Int)
   in fmap (\v -> v + (i * m v))
{-# INLINE zone #-}

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
        log_ logger (Text.pack err) >> threadDelay 30000000
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
        threadDelay 30000000

-- IMAGE

data Image

instance Accept Image where
  contentType _ =
    "image" Media.// "jpg"

instance MimeRender Image ByteString where
  mimeRender _ = id

-- PROFILE

timed :: PositiveM sig m => Text -> a -> m a
timed name action = do
  log $ name <> " - started"
  start <- sendIO Time.getCurrentTime
  a <- sendIO $ evaluate action
  done <- sendIO Time.getCurrentTime
  log $ name <> " - processed in: " <> tshow (Time.diffUTCTime done start)
  pure a

-- HELPERS

workingDirectory :: PositiveM sig m => m FilePath
workingDirectory =
  asks (segmentToString . eDir)

pSwapMVar :: MVar a -> a -> IO ()
pSwapMVar mvar a = do
  isEmpty <- isEmptyMVar mvar
  print isEmpty
  if isEmpty
    then putMVar mvar a
    else void $ swapMVar mvar a
