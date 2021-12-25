{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Server.Handler where

import qualified Control.Concurrent.MVar as MVar
import Control.Effect.Labelled hiding (Handler)
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Effect.Throw
import Data.ByteString.Lazy (ByteString)
import qualified Data.Massiv.Array as Massiv
import qualified Data.Massiv.Array.Manifest.Vector as Massiv
import qualified Data.Text as Text
import qualified Graphics.Image as HIP
import qualified Positive.CLI as CLI
import Positive.Data.Collection (Collection)
import Positive.Data.FilmRoll (FilmRoll)
import Positive.Data.Id
import Positive.Data.ImageSettings (
  CoordinateInfo (..),
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
import qualified System.Exit
import System.FilePath.Posix (isPathSeparator, (</>))
import qualified System.Process as Process


-- HANDLER

type Handler sig m =
  ( HasLabelled "stdout" Log sig m
  , HasLabelled "sse" Log sig m
  , Has PostgreSQL sig m
  , Has (Reader Env) sig m
  , Has (Lift IO) sig m
  , Has (Throw PostgreSQL.Error) sig m
  , Has (Throw ServerError) sig m
  )


data Env = Env
  { imageMVar :: !(MVar (Maybe (ImageSettingsId, ImageCrop, Image.Monochrome)))
  , previewMVar :: !(MVar ())
  , isDev :: !CLI.IsDev
  }


-- IMAGE

handleImage :: Handler sig m => Text -> ImageSettings -> m ByteString
handleImage dir settings =
  withContext @"stdout" "image-handler" $ do
    env <- ask @Env
    (image, putMVarBack) <-
      getCachedImage settings.id settings.crop $
        Text.pack (Text.unpack dir </> Path.toFilePath settings.filename)
    if env.isDev
      then do
        processed <- timed "apply" $ Image.applySettings settings image
        encoded <- timed "encode" =<< sendIO (Image.encode "_.png" processed)
        sendIO putMVarBack
        pure encoded
      else do
        logTraceShow @"stdout" "applying settings and encode" settings.id
        !encoded <- sendIO . Image.encode "_.png" $ Image.applySettings settings image
        sendIO putMVarBack
        pure encoded


-- SAVE

handleSaveFilmRoll :: Handler sig m => FilmRoll -> m FilmRoll
handleSaveFilmRoll filmRoll = do
  _ <-
    PostgreSQL.runTransaction $ do
      Session.updateImageSettingsList filmRoll.imageSettings
      Session.updatePoster filmRoll.poster filmRoll.id
  logTraceShow @"stdout" "saved filmroll" filmRoll.id
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
  logTraceShow @"stdout" "generating wallpaper version" settings.id
  output <-
    sendIO $
      Util.ensureUniqueFilename . outputBase =<< sendIO Directory.getHomeDirectory
  image <-
    throwLeft
      . sendIO
      $ Image.fromDiskPreProcess (Just 2560) settings.crop input
  sendIO . HIP.writeImage output $ Image.applySettings settings image
  logTraceShow @"stdout" "wrote wallpaper version" settings.id
  pure NoContent


-- OPEN

handleOpenExternalEditor :: Handler sig m => ImageSettings -> m NoContent
handleOpenExternalEditor settings = do
  directoryPath <-
    PostgreSQL.runSession $ Session.selectDirectoryPath settings.id
  let input = Path.append directoryPath settings.filename
  logTraceShow @"stdout" "opening in external editor" settings.id
  image <-
    throwLeft
      . sendIO
      $ Image.fromDiskPreProcess Nothing settings.crop input
  sendIO
    . HIP.displayImageUsing HIP.defaultViewer False
    $ Image.applySettings settings image
  pure NoContent


handleOpenInFinder :: Handler sig m => ImageSettingsId -> m NoContent
handleOpenInFinder imageSettingsId = do
  directoryPath <-
    fmap Path.toFilePath
      . PostgreSQL.runSession
      $ Session.selectDirectoryPath imageSettingsId
  logTraceShow @"stdout" "opening in finder" imageSettingsId
  result <- sendIO . Process.withCreateProcess (Process.proc "open" [directoryPath]) $
    \_ _ _ handler ->
      Process.waitForProcess handler >>= \case
        System.Exit.ExitSuccess ->
          pure $ Right "Successfully opened directory in finder"
        result -> pure $ Left (tshow result)
  either
    (logError @"stdout" "open directory in finder")
    (logTrace @"stdout" "open directory in finder")
    result
  pure NoContent


-- HISTOGRAM

handleGetSettingsHistogram :: Handler sig m => ImageSettings -> m (Vector Int)
handleGetSettingsHistogram settings = do
  directoryPath <-
    PostgreSQL.runSession $ Session.selectDirectoryPath settings.id
  (image, putMVarBack) <-
    getCachedImage settings.id settings.crop $
      Text.pack (Path.append directoryPath settings.filename)
  sendIO putMVarBack
  logTraceShow @"stdout" "creating histogram for" settings.id
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
            <$> getCachedImage settings.id settings.crop
              (Text.pack (Path.append directoryPath settings.filename))
        sendIO putMVarBack
        pure $ fmap (toInfo image) coordinates


-- LIST DIRECTORIES

handleGetSettings :: Handler sig m => m [FilmRoll]
handleGetSettings = do
  logTrace @"stdout" "handler" "get filmrolls"
  PostgreSQL.runSession Session.selectFilmRolls


handleGetCollections :: Handler sig m => m [Collection]
handleGetCollections = do
  logTrace @"stdout" "handler" "get collections"
  PostgreSQL.runSession Session.selectCollections


handleAddToCollection ::
  Handler sig m => CollectionId -> ImageSettingsId -> m [Collection]
handleAddToCollection collectionId imageSettingsId = do
  logTraceShow @"stdout" "add to collection" (collectionId, imageSettingsId)
  PostgreSQL.runSession $ do
    Session.insertImageToCollection collectionId imageSettingsId
    Session.selectCollections


handleRemoveFromCollection ::
  Handler sig m => CollectionId -> ImageSettingsId -> m [Collection]
handleRemoveFromCollection collectionId imageSettingsId = do
  logTraceShow @"stdout" "remove from collection" (collectionId, imageSettingsId)
  PostgreSQL.runSession $ do
    Session.deleteImageFromCollection collectionId imageSettingsId
    Session.selectCollections


handleSetCollectionTarget :: Handler sig m => CollectionId -> m [Collection]
handleSetCollectionTarget collectionId = do
  logTraceShow @"stdout" "set collection target" collectionId
  PostgreSQL.runTransaction $
    Session.updateCollectionTarget collectionId
  PostgreSQL.runSession Session.selectCollections


-- HANDLER HELPERS

throwLeft :: Show err => Handler sig m => m (Either err a) -> m a
throwLeft m =
  m
    >>= either
      (\(e :: err) -> logErrorShow @"stdout" "error" e >> throwError err404)
      pure


-- | Read image from disk, normalize before crop, keep result in MVar
getCachedImage ::
  Handler sig m =>
  ImageSettingsId ->
  ImageCrop ->
  Text ->
  m (Image.Monochrome, IO ())
getCachedImage imageSettingsId crop path =
  withContext @"stdout" "image-cache" $ do
    env <- ask @Env
    maybeCache <- sendIO $ MVar.takeMVar env.imageMVar
    case maybeCache of
      Just cache@(cachedImageSettingsId, cachedCrop, cachedImage)
        | cachedCrop == crop && cachedImageSettingsId == imageSettingsId -> do
          logTraceShow @"stdout" "loaded from cache" imageSettingsId
          pure (cachedImage, MVar.putMVar env.imageMVar (Just cache))
        | otherwise -> do
          image <- throwLeft $ Util.readImageFromWithCache imageSettingsId crop path
          Util.writeToCache cachedImageSettingsId cachedCrop cachedImage
          pure
            ( image
            , MVar.putMVar env.imageMVar (Just (imageSettingsId, crop, image))
            )
      Nothing -> do
        logTraceShow @"stdout" "loading from disk" imageSettingsId
        image <- throwLeft $ Util.readImageFromWithCache imageSettingsId crop path
        pure
          ( image
          , MVar.putMVar env.imageMVar (Just (imageSettingsId, crop, image))
          )
