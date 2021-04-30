{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Import where

import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Hasql.Connection
import qualified Hasql.Session
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..))
import qualified Hasql.Transaction.Sessions as Transaction
import Positive.Database.Session
import Positive.Filename
import Positive.FilmRoll (FilmRoll)
import qualified Positive.Image.Util as Util
import Positive.Prelude hiding (ByteString)
import System.Directory

-- IMPORT

run :: (Text -> IO ()) -> IO ()
run log = do
  filmRolls <- makePathsAbsolute =<< Util.findImageSettings
  Right conn <- Hasql.Connection.acquire "host=localhost port=5432 dbname=positive"
  result <-
    Hasql.Session.run
      (Transaction.transaction Serializable Write (session filmRolls))
      conn
  Hasql.Connection.release conn
  log $ tshow result

makePathsAbsolute :: HashMap Text FilmRoll -> IO [(Text, FilmRoll)]
makePathsAbsolute filmRolls =
  forM (HashMap.toList filmRolls) $ \(path, filmRoll) -> do
    absolutePath <- liftIO $ makeAbsolute (Text.unpack path)
    pure (Text.pack absolutePath, filmRoll)

session :: [(Text, FilmRoll)] -> Transaction ()
session filmRolls =
  for_ filmRolls $ \(path, filmRoll) -> do
    filmRollId <- insertFilmRoll path
    images <- for (HashMap.toList filmRoll.frsSettings) $ \(_, settings) ->
      insertImageSettings filmRollId settings filmRoll.frsRatings
    let poster =
          fst
            <$> List.find
              (\(_, filename) -> Just filename == fmap toText filmRoll.frsPoster)
              images
    updatePoster poster filmRollId
