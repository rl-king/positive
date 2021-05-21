{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Server.Handler where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
import Control.Effect.Labelled hiding (Handler)
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Effect.Throw
import Control.Exception (evaluate)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Massiv.Array as Massiv
import qualified Data.Massiv.Array.Manifest.Vector as Massiv
import qualified Data.OrdPSQ as OrdPSQ
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified Graphics.Image as HIP
import qualified Positive.CLI as CLI
import Positive.Data.Collection (Collection)
import Positive.Data.FilmRoll (FilmRoll)
import Positive.Data.Id
import Positive.Data.ImageSettings
  ( CoordinateInfo (..),
    Expression (..),
    ExpressionResult (..),
    ImageCrop (..),
    ImageSettings,
  )
import qualified Positive.Data.Path as Path
import qualified Positive.Database.Session as Session
import Positive.Effect.Log
import Positive.Effect.PostgreSQL (PostgreSQL)
import qualified Positive.Effect.PostgreSQL as PostgreSQL
import qualified Positive.Image as Image
import qualified Positive.Image.Util as Util
import qualified Positive.Language as Language
import qualified Positive.Metadata as Metadata
import Positive.Prelude hiding (ByteString)
import qualified Positive.SingleImage as SingleImage
import Positive.Timed
import Servant hiding (Handler, throwError)
import qualified System.Directory as Directory
import System.FilePath.Posix (isPathSeparator, (</>))

-- HANDLER

type Handler sig m =
  ( HasLabelled "stdout" Log sig m,
    HasLabelled "sse" Log sig m,
    Has PostgreSQL sig m,
    Has (Reader Env) sig m,
    Has (Lift IO) sig m,
    Has (Throw PostgreSQL.Error) sig m,
    Has (Throw ServerError) sig m
  )

data Env = Env
  { imageMVar :: !(MVar (OrdPSQ Text UTCTime (ImageCrop, Image.Monochrome))),
    previewMVar :: !(MVar ()),
    isDev :: !CLI.IsDev
  }

-- IMAGE

handleImage :: Handler sig m => Text -> ImageSettings -> m ByteString
handleImage dir settings = do
  env <- ask @Env
  (image, putMVarBack) <-
    getCachedImage settings.crop $
      Text.pack (Text.unpack dir </> Path.toFilePath settings.filename)
  if env.isDev
    then do
      processed <- timed "apply" $ Image.applySettings settings image
      encoded <- timed "encode" =<< sendIO (Image.encode "_.png" processed)
      sendIO putMVarBack
      pure encoded
    else do
      logInfo @"stdout" "handler" $
        "applying settings and encode: " <> Path.unpack settings.filename
      !encoded <- sendIO . Image.encode "_.png" $ Image.applySettings settings image
      sendIO putMVarBack
      pure encoded

-- SAVE

handleSaveFilmRoll :: Handler sig m => FilmRoll -> m FilmRoll
handleSaveFilmRoll filmRoll = do
  _ <-
    PostgreSQL.runTransaction $
      Session.updateImageSettingsList filmRoll.imageSettings
  logDebug @"stdout" "handler" "saved filmroll"
  env <- ask @Env
  void . sendIO $
    MVar.tryPutMVar env.previewMVar ()
  pure filmRoll

-- CHECK EXPRESSIONS

handleCheckExpressions :: Handler sig m => [Expression] -> m [ExpressionResult]
handleCheckExpressions exprs =
  let eval v expr =
        SampleEval $
          filter
            (not . isInfinite)
            [Language.eval p v expr - p | p <- [0.1, 0.2 .. 1.0]]
      parseAndCheck e =
        first TypeError . Language.check
          =<< first SyntaxError (Language.parse e.eExpr)
   in pure [either identity (eval e.eValue) (parseAndCheck e) | e <- exprs]

-- GENERATE

handleGenerateHighRes :: Handler sig m => ImageSettings -> m NoContent
handleGenerateHighRes settings = do
  directoryPath <-
    PostgreSQL.runSession $ Session.selectDirectoryPath settings.id
  SingleImage.generate directoryPath settings
  pure NoContent

handleGenerateWallpaper :: Handler sig m => ImageSettings -> m NoContent
handleGenerateWallpaper settings = do
  directoryPath <-
    PostgreSQL.runSession $ Session.selectDirectoryPath settings.id
  let input =
        Path.append directoryPath settings.filename
      outputBase homeDir =
        homeDir
          </> "Documents/wallpapers/positive"
          </> filter
            (\c -> not (isPathSeparator c || c == '.'))
            (Path.toFilePath directoryPath)
          <> " | "
          <> Path.toFilePath settings.filename
  logInfo @"stdout" "handler" $ "generating wallpaper version of: " <> Text.pack input
  output <-
    sendIO $
      Util.ensureUniqueFilename . outputBase =<< sendIO Directory.getHomeDirectory
  image <-
    handleLeft
      . sendIO
      $ Image.fromDiskPreProcess (Just 2560) settings.crop input
  sendIO . HIP.writeImage output $ Image.applySettings settings image
  logInfo @"stdout" "handler" ("wrote wallpaper version of: " <> Text.pack input)
  pure NoContent

-- OPEN EXTERNALEDITOR

handleOpenExternalEditor :: Handler sig m => ImageSettings -> m NoContent
handleOpenExternalEditor settings = do
  directoryPath <-
    PostgreSQL.runSession $ Session.selectDirectoryPath settings.id
  let input = Path.append directoryPath settings.filename
  logInfo @"stdout" "handler" $ "ppening in external editor: " <> Text.pack input
  image <-
    handleLeft
      . sendIO
      $ Image.fromDiskPreProcess Nothing settings.crop input
  sendIO
    . HIP.displayImageUsing HIP.defaultViewer False
    $ Image.applySettings settings image
  pure NoContent

-- HISTOGRAM

handleGetSettingsHistogram :: Handler sig m => ImageSettings -> m (Vector Int)
handleGetSettingsHistogram settings = do
  directoryPath <-
    PostgreSQL.runSession $ Session.selectDirectoryPath settings.id
  (image, putMVarBack) <-
    getCachedImage settings.crop $
      Text.pack (Path.append directoryPath settings.filename)
  sendIO putMVarBack
  logDebug @"stdout" "handler" $
    "creating histogram for: " <> Path.unpack settings.filename
  pure . Massiv.toVector . Metadata.generateHistogram . HIP.unImage $
    Image.applySettings settings image

-- COORDINATE

handleGetCoordinateInfo ::
  Handler sig m =>
  ([(Double, Double)], ImageSettings) ->
  m [CoordinateInfo]
handleGetCoordinateInfo (coordinates, settings) =
  let toInfo image (x, y) =
        CoordinateInfo x y
          . (\(HIP.PixelY p) -> HIP.toDouble p)
          . HIP.borderIndex Massiv.Continue image
          $ HIP.Ix2
            (floor (int2Double (HIP.rows image) * y))
            (floor (int2Double (HIP.cols image) * x))
   in do
        directoryPath <-
          PostgreSQL.runSession $ Session.selectDirectoryPath settings.id
        (image, putMVarBack) <-
          first (Image.applySettings settings)
            <$> getCachedImage settings.crop
              (Text.pack (Path.append directoryPath settings.filename))
        sendIO putMVarBack
        pure $ fmap (toInfo image) coordinates

-- LIST DIRECTORIES

handleGetSettings :: Handler sig m => m [FilmRoll]
handleGetSettings = do
  logInfo @"stdout" "handler" "get filmrolls"
  PostgreSQL.runSession Session.selectFilmRolls

handleGetCollections :: Handler sig m => m [Collection]
handleGetCollections = do
  logInfo @"stdout" "handler" "get collections"
  PostgreSQL.runSession Session.selectCollections

handleAddToCollection ::
  Handler sig m => CollectionId -> ImageSettingsId -> m [Collection]
handleAddToCollection collectionId imageSettingsId = do
  logInfo @"stdout" "handler" "add to collection"
  PostgreSQL.runSession $ do
    Session.insertImageToCollection collectionId imageSettingsId
    Session.selectCollections

handleRemoveFromCollection ::
  Handler sig m => CollectionId -> ImageSettingsId -> m [Collection]
handleRemoveFromCollection collectionId imageSettingsId = do
  logInfo @"stdout" "handler" "remove from collection"
  PostgreSQL.runSession $ do
    Session.deleteImageFromCollection collectionId imageSettingsId
    Session.selectCollections

-- HANDLER HELPERS

handleLeft :: Handler sig m => m (Either err a) -> m a
handleLeft m =
  m >>= either (\(_ :: err) -> logInfo @"stdout" "handler" "image read error" >> throwError err404) pure

-- | Read image from disk, normalize before crop, keep result in MVar
getCachedImage :: Handler sig m => ImageCrop -> Text -> m (Image.Monochrome, IO ())
getCachedImage crop path = do
  env <- ask @Env
  now <- sendIO Time.getCurrentTime
  cache <- sendIO $ MVar.takeMVar env.imageMVar
  logInfo @"stdout" "handler" $ "cached images: " <> tshow (OrdPSQ.size cache)
  case checkCrop crop =<< OrdPSQ.lookup path cache of
    Just (_, cached@(_, loadedImage)) -> do
      logDebug @"stdout" "handler" "loaded image from cache"
      pure
        ( loadedImage,
          MVar.putMVar env.imageMVar
            =<< evaluate (DeepSeq.force (insertAndTrim path now cached cache))
        )
    Nothing -> do
      logDebug @"stdout" "handler" "loadin image from disk"
      image <-
        handleLeft . sendIO $
          Image.fromDiskPreProcess (Just 1440) crop (Text.unpack path)
      pure
        ( image,
          MVar.putMVar env.imageMVar
            =<< evaluate (DeepSeq.force (insertAndTrim path now (crop, image) cache))
        )

checkCrop ::
  ImageCrop ->
  (UTCTime, (ImageCrop, Image.Monochrome)) ->
  Maybe (UTCTime, (ImageCrop, Image.Monochrome))
checkCrop crop cached@(_, (cachedCrop, _))
  | crop == cachedCrop = Just cached
  | otherwise = Nothing

insertAndTrim :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
insertAndTrim k v p psq =
  let q = OrdPSQ.insert k v p psq
   in if OrdPSQ.size q > 40 then OrdPSQ.deleteMin q else q
