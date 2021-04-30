{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Database.Session where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction as Transaction
import qualified Positive.Database.Statement as Statement
import Positive.Filename
import Positive.Image.Settings
import Positive.Prelude

insertImageSettings :: Int32 -> Settings -> HashMap Filename Int -> Transaction (Int32, Text)
insertImageSettings filmRollId settings ratings =
  Transaction.statement (filmRollId, settings, ratings) $
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

insertFilmRoll :: Text -> Transaction Int32
insertFilmRoll path =
  Transaction.statement path Statement.insertFilmRoll

updatePoster :: Maybe Int32 -> Int32 -> Transaction Int32
updatePoster imageId filmRollId =
  Transaction.statement (imageId, filmRollId) Statement.updatePoster
