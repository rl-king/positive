{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Positive.Init where

import Control.Carrier.Error.Church as Error.Church
import Control.Effect.Labelled
import Control.Effect.Lift
import qualified Data.Text as Text
import qualified Hasql.Pool as Hasql
import Positive.Data.Id
import qualified Positive.Data.Path as Path
import qualified Positive.Database.Session as Session
import Positive.Effect.Log
import Positive.Effect.PostgreSQL (PostgreSQL)
import qualified Positive.Effect.PostgreSQL as PostgreSQL
import qualified Positive.Metadata as Metadata
import Positive.Prelude hiding (ByteString)
import System.Directory
import System.FilePath.Posix

run :: TimedFastLogger -> Hasql.Pool -> IO ()
run logger pool = do
  dir <- Path.fromFilePath <$> getCurrentDirectory
  filenames <-
    fmap Path.fromFilePath
      . filter (\x -> isExtensionOf ".tif" x || isExtensionOf ".png" x)
      <$> listDirectory "."
  insertNewFilmRoll dir filenames >>= generatePreviews dir
    & PostgreSQL.runPostgreSQL pool
    & Error.Church.runError @PostgreSQL.Error
      (logError @"stdout" "preview" . tshow)
      pure
    & runLabelled @"stdout"
    & runLogStdout logger

insertNewFilmRoll ::
  ( HasLabelled "stdout" Log sig m,
    Has PostgreSQL sig m,
    Has (Lift IO) sig m,
    Has (Throw PostgreSQL.Error) sig m
  ) =>
  Path.Directory ->
  [Path.Filename] ->
  m [ImageSettingsId]
insertNewFilmRoll dir filenames = do
  imageSettingsIds <- PostgreSQL.runTransaction $ do
    filmRollId <- Session.insertFilmRoll dir
    traverse (Session.insertImageSettings filmRollId) filenames
  logInfo @"stdout" "init" $ "inserted film roll for: " <> Path.unpack dir
  logInfo @"stdout" "init" $
    Text.unwords
      [ "inserted image settings for:",
        tshow (length imageSettingsIds),
        "image(s)"
      ]
  sendIO $ createDirectoryIfMissing False "preview"
  pure imageSettingsIds

generatePreviews ::
  ( HasLabelled "stdout" Log sig m,
    Has PostgreSQL sig m,
    Has (Lift IO) sig m,
    Has (Throw PostgreSQL.Error) sig m
  ) =>
  Path.Directory ->
  [ImageSettingsId] ->
  m ()
generatePreviews dir imageSettingsIds = do
  logInfo @"stdout" "init" $
    "generating " <> tshow (length imageSettingsIds) <> " previews"
  for_ imageSettingsIds $ \imageSettingsId -> do
    imageSettings <-
      PostgreSQL.runSession $ Session.selectImageSettings imageSettingsId
    Metadata.upsertMetadata dir imageSettings
