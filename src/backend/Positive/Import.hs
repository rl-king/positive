{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Import where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Hasql.Connection
import Hasql.Session (Session)
import qualified Hasql.Session
import Positive.Database.Session
import Positive.FilmRoll (FilmRoll)
import qualified Positive.Image.Util as Util
import Positive.Prelude hiding (ByteString)
import System.Directory

-- IMPORT

run :: (Text -> IO ()) -> IO ()
run log = do
  filmrolls <- Util.findImageSettings
  Right conn <- Hasql.Connection.acquire "host=localhost port=5432 dbname=positive"
  result <- Hasql.Session.run (session log filmrolls) conn
  Hasql.Connection.release conn
  log $ tshow result

session :: (Text -> IO ()) -> HashMap Text FilmRoll -> Session ()
session log filmrolls =
  for_ (HashMap.toList filmrolls) $ \(path, filmroll) -> do
    absolutePath <- liftIO $ makeAbsolute (Text.unpack path)
    filmRollId <- insertFilmRoll (Text.pack absolutePath) filmroll
    for_ (HashMap.toList filmroll.frsSettings) $ \(_, settings) ->
      insertImageSettings filmRollId settings filmroll.frsRatings
