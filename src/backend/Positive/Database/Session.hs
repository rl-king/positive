{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Database.Session where

import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import Hasql.Statement (refineResult)
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction as Transaction
import Positive.Data.Filename
import Positive.Data.FilmRoll
import Positive.Data.ImageSettings
import qualified Positive.Database.Statement as Statement
import Positive.Prelude

-- INSERT

insertImageSettings ::
  Int32 ->
  ImageSettings ->
  HashMap Filename Int ->
  Transaction (Int32, Text)
insertImageSettings filmRollId imageSettings ratings =
  Transaction.statement (filmRollId, imageSettings, ratings) $
    lmap
      ( \(frid, s, r) ->
          ( toText s.filename,
            toEnum . fromMaybe 0 $ HashMap.lookup s.filename r,
            s.rotate,
            Aeson.toJSON s.crop,
            s.gamma,
            Aeson.toJSON s.zones,
            s.blackpoint,
            s.whitepoint,
            Aeson.toJSON s.expressions,
            frid
          )
      )
      Statement.insertImageSettings

insertFilmRoll :: Text -> Transaction Int32
insertFilmRoll path =
  Transaction.statement path Statement.insertFilmRoll

-- UPDATE

updateFilmRoll :: FilmRoll -> Transaction ()
updateFilmRoll filmRoll =
  traverse_ (updateImageSettings 1) $ HashMap.elems filmRoll.frsSettings

updateImageSettings :: Int32 -> ImageSettings -> Transaction Int32
updateImageSettings imageId imageSettings =
  Transaction.statement (imageId, imageSettings) $
    lmap
      ( \(iId, s) ->
          ( iId,
            s.rotate,
            Aeson.toJSON s.crop,
            s.gamma,
            Aeson.toJSON s.zones,
            s.blackpoint,
            s.whitepoint,
            Aeson.toJSON s.expressions
          )
      )
      Statement.updateImageSettings

updatePoster :: Maybe Int32 -> Int32 -> Transaction Int32
updatePoster imageId filmRollId =
  Transaction.statement (imageId, filmRollId) Statement.updatePoster

-- SELECT

selectFilmRolls :: Session (HashMap Text FilmRoll)
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
                      ImageSettings
                        { filename = Filename filename,
                          rotate = orientation,
                          crop = crop,
                          gamma = gamma,
                          zones = zones,
                          blackpoint = blackpoint,
                          whitepoint = whitepoint,
                          expressions = expressions
                        }
                }
            )
      toHashMap =
        bimap Text.pack (HashMap.fromListWith (<>)) . traverse toPair
   in Session.statement () $
        refineResult (toHashMap . Vector.toList) Statement.selectFilmRolls
