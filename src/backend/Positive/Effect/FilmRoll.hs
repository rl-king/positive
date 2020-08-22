{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Positive.Effect.FilmRoll where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Effect.Throw
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import Positive.Prelude
import Positive.Settings
import System.Directory
import System.FilePath.Posix ((</>))
import qualified System.FilePath.Posix as Path

data FilmRoll (m :: Type -> Type) k where
  Read :: Maybe PathSegment -> FilmRoll m FilmRollSettings
  Write :: Maybe PathSegment -> FilmRollSettings -> FilmRoll m ()

readSettings :: Has FilmRoll sig m => m FilmRollSettings
readSettings = send (Read Nothing)

writeSettings :: Has FilmRoll sig m => FilmRollSettings -> m ()
writeSettings = send . Write Nothing

readPreviewSettings :: Has FilmRoll sig m => m FilmRollSettings
readPreviewSettings = send (Read (Just (PathSegment "previews")))

writePreviewSettings :: Has FilmRoll sig m => FilmRollSettings -> m ()
writePreviewSettings = send . Write (Just (PathSegment "previews"))

newtype FilmRollC m a = FilmRollC {unFilmRollC :: ReaderC PathSegment m a}
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, Has (Lift IO) sig m, Has (Throw String) sig m) => Algebra (FilmRoll :+: sig) (FilmRollC m) where
  alg hdl sig ctx =
    case sig of
      R other ->
        FilmRollC (alg (unFilmRollC . hdl) (R other) ctx)
      L (Read maybeDir) -> do
        dir <- FilmRollC ask
        fmap (<$ ctx) . either throwError pure =<< sendIO (getSettingsFile (maybe dir (dir <>) maybeDir))
      L (Write maybeDir settings) -> do
        dir <- FilmRollC ask
        let path = maybe dir (dir <>) maybeDir <> PathSegment "image-settings.json"
        fmap (<$ ctx) . sendIO . ByteString.writeFile (segmentToString path) $ Aeson.encode settings

runFilmRoll :: PathSegment -> FilmRollC m a -> m a
runFilmRoll dir =
  runReader dir . unFilmRollC

--

getSettingsFile :: PathSegment -> IO (Either String FilmRollSettings)
getSettingsFile (PathSegment dir) = do
  let path = Text.unpack dir </> "image-settings.json"
  exists <- doesPathExist path
  if exists
    then Aeson.eitherDecodeFileStrict path
    else do
      filenames <- getAllPngs $ Text.unpack dir
      let settings = fromFilenames filenames
      ByteString.writeFile path $ Aeson.encode settings
      pure (Right settings)

getAllPngs :: FilePath -> IO [Text]
getAllPngs dir =
  fmap Text.pack . filter ((".png" ==) . Path.takeExtension) <$> listDirectory dir
