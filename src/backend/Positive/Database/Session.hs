{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Database.Session where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import qualified Positive.Database.Statement as Statement
import Positive.Filename
import Positive.FilmRoll
import Positive.Image.Settings
import Positive.Prelude

insertImageSettings :: Int32 -> Settings -> HashMap Filename Int -> Session Int32
insertImageSettings filmRollId settings ratings =
  Session.statement (filmRollId, settings, ratings) $
    lmap
      ( \(frid, s, r) ->
          ( toText s.iFilename,
            toEnum . fromMaybe 0 $ HashMap.lookup s.iFilename r,
            s.iRotate,
            toJSON s.iCrop,
            s.iGamma,
            toJSON s.iZones,
            s.iBlackpoint,
            s.iWhitepoint,
            toJSON s.iExpressions,
            frid
          )
      )
      Statement.insertImageSettings

insertFilmRoll :: Text -> FilmRoll -> Session Int32
insertFilmRoll path filmRoll =
  Session.statement
    (path, fmap toText filmRoll.frsPoster)
    Statement.insertFilmRoll
