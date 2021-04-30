{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Database.Session where

import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Hasql.Statement (refineResult)
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction as Transaction
import qualified Positive.Database.Statement as Statement
import Positive.Filename
import Positive.FilmRoll
import Positive.Image.Settings
import Positive.Prelude

-- INSERT

insertImageSettings :: Int32 -> Settings -> HashMap Filename Int -> Transaction (Int32, Text)
insertImageSettings filmRollId settings ratings =
  Transaction.statement (filmRollId, settings, ratings) $
    lmap
      ( \(frid, s, r) ->
          ( toText s.iFilename,
            toEnum . fromMaybe 0 $ HashMap.lookup s.iFilename r,
            s.iRotate,
            Aeson.toJSON s.iCrop,
            s.iGamma,
            Aeson.toJSON s.iZones,
            s.iBlackpoint,
            s.iWhitepoint,
            Aeson.toJSON s.iExpressions,
            frid
          )
      )
      Statement.insertImageSettings

insertFilmRoll :: Text -> Transaction Int32
insertFilmRoll path =
  Transaction.statement path Statement.insertFilmRoll

-- UPDATE

updatePoster :: Maybe Int32 -> Int32 -> Transaction Int32
updatePoster imageId filmRollId =
  Transaction.statement (imageId, filmRollId) Statement.updatePoster

-- SELECT

selectFilmRolls :: Transaction (HashMap Text FilmRoll)
selectFilmRolls =
  let toPair
        ( _filmRollId,
          path,
          poster,
          _imageId,
          filename,
          rating,
          orientation,
          cropValue,
          gamma,
          zonesValue,
          blackpoint,
          whitepoint,
          expressionsValue
          ) = do
          crop <- Aeson.parseEither Aeson.parseJSON cropValue
          zones <- Aeson.parseEither Aeson.parseJSON zonesValue
          expressions <- Aeson.parseEither Aeson.parseJSON expressionsValue
          pure
            ( path,
              FilmRoll
                { frsPoster = fmap Filename poster,
                  frsRatings =
                    HashMap.singleton (Filename filename) $ fromEnum rating,
                  frsSettings =
                    HashMap.singleton
                      (Filename filename)
                      Settings
                        { iFilename = Filename filename,
                          iRotate = orientation,
                          iCrop = crop,
                          iGamma = gamma,
                          iZones = zones,
                          iBlackpoint = blackpoint,
                          iWhitepoint = whitepoint,
                          iExpressions = expressions
                        }
                }
            )
      toHashMap =
        bimap Text.pack (HashMap.fromListWith (<>)) . traverse toPair
   in Transaction.statement () $
        refineResult (toHashMap . Vector.toList) Statement.selectFilmRolls
