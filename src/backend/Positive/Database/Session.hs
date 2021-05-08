{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

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
import Positive.Data.FilmRoll
import Positive.Data.Id
import qualified Positive.Data.Id as Id
import Positive.Data.ImageSettings
import qualified Positive.Data.Path as Path
import qualified Positive.Database.Statement as Statement
import Positive.Prelude

-- INSERT

insertImageSettings :: FilmRollId -> Path.Filename -> Transaction (ImageSettingsId, Text)
insertImageSettings filmRollId filename =
  let toInitalSettings (filmRollId, filename) =
        ( Path.unpack filename,
          0,
          0,
          Aeson.toJSON emptyCrop,
          2.2,
          Aeson.toJSON emptyZones,
          0,
          1,
          Aeson.toJSON @[Expression] [],
          Id.unpack filmRollId
        )
   in Transaction.statement (filmRollId, filename) $
        dimap toInitalSettings (first Id.pack) Statement.insertImageSettings

insertFilmRoll :: Text -> Transaction FilmRollId
insertFilmRoll path =
  Transaction.statement path $
    rmap Id.pack Statement.insertFilmRoll

-- UPDATE

updateFilmRoll :: FilmRoll -> Transaction ()
updateFilmRoll =
  traverse_ updateImageSettings . imageSettings

updateImageSettings :: ImageSettings -> Transaction Int32
updateImageSettings imageSettings =
  Transaction.statement imageSettings $
    lmap
      ( \s ->
          ( Id.unpack s.id,
            s.rotate,
            Aeson.toJSON s.crop,
            s.gamma,
            Aeson.toJSON s.zones,
            s.blackpoint,
            s.whitepoint,
            Aeson.toJSON s.expressions,
            s.rating
          )
      )
      Statement.updateImageSettings

updatePreviewTimestamp :: ImageSettingsId -> Session ImageSettingsId
updatePreviewTimestamp imageSettingsId =
  Session.statement imageSettingsId $
    dimap Id.unpack Id.pack Statement.updatePreviewTimestamp

updatePoster :: Maybe ImageSettingsId -> FilmRollId -> Transaction FilmRollId
updatePoster imageSettingsId filmRollId =
  Transaction.statement (imageSettingsId, filmRollId) $
    dimap (bimap (fmap Id.unpack) Id.unpack) Id.pack Statement.updatePoster

-- SELECT

selectFilmRolls :: Session [FilmRoll]
selectFilmRolls =
  let merge newFilmRoll acc =
        HashMap.alter
          ( \case
              Nothing ->
                Just newFilmRoll
              Just existingFilmRoll ->
                Just $
                  existingFilmRoll
                    { poster = newFilmRoll.poster <|> existingFilmRoll.poster,
                      imageSettings =
                        newFilmRoll.imageSettings <> existingFilmRoll.imageSettings
                    }
          )
          newFilmRoll.id
          acc
      toFilmRolls =
        fmap (HashMap.elems . foldr merge mempty) . traverse filmRollFromTuple
   in Session.statement () $
        refineResult toFilmRolls Statement.selectFilmRolls

selectFilmRoll :: FilmRollId -> Session FilmRoll
selectFilmRoll filmRollId =
  Session.statement filmRollId
    . lmap Id.unpack
    $ refineResult filmRollFromTuple Statement.selectFilmRoll

selectFilmRollByImageSettings :: ImageSettingsId -> Session FilmRoll
selectFilmRollByImageSettings imageSettingsId =
  Session.statement imageSettingsId
    . lmap Id.unpack
    $ refineResult filmRollFromTuple Statement.selectFilmRollByImageSettings

selectOutdatedPreviews :: Session [(Path.Directory, ImageSettings)]
selectOutdatedPreviews =
  Session.statement () $
    refineResult
      (traverse imageSettingsFromTuple . Vector.toList)
      Statement.selectOutdatedPreviews

selectImageSettingsByPath ::
  Path.Directory ->
  Path.Filename ->
  Session (Path.Directory, ImageSettings)
selectImageSettingsByPath dir filename =
  Session.statement (dir, filename) . lmap (bimap Path.unpack Path.unpack) $
    refineResult imageSettingsFromTuple Statement.selectImageSettingsByPath

-- MAPPING

filmRollFromTuple :: _ -> Either Text FilmRoll
filmRollFromTuple
  ( filmRollId,
    directoryPath,
    poster,
    imageSettingsId,
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
    crop <- first Text.pack $ Aeson.parseEither Aeson.parseJSON cropValue
    zones <- first Text.pack $ Aeson.parseEither Aeson.parseJSON zonesValue
    expressions <- first Text.pack $ Aeson.parseEither Aeson.parseJSON expressionsValue
    pure $
      FilmRoll
        { id = Id.pack filmRollId,
          directoryPath = Path.pack directoryPath,
          poster = fmap Id.pack poster,
          imageSettings =
            [ ImageSettings
                { id = Id.pack imageSettingsId,
                  filename = Path.pack filename,
                  rating = rating,
                  rotate = orientation,
                  crop = crop,
                  gamma = gamma,
                  zones = zones,
                  blackpoint = blackpoint,
                  whitepoint = whitepoint,
                  expressions = expressions
                }
            ]
        }

imageSettingsFromTuple :: _ -> Either Text (Path.Directory, ImageSettings)
imageSettingsFromTuple
  ( directoryPath,
    imageSettingsId,
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
    crop <- first Text.pack $ Aeson.parseEither Aeson.parseJSON cropValue
    zones <- first Text.pack $ Aeson.parseEither Aeson.parseJSON zonesValue
    expressions <- first Text.pack $ Aeson.parseEither Aeson.parseJSON expressionsValue
    pure
      ( Path.pack directoryPath,
        ImageSettings
          { id = Id.pack imageSettingsId,
            filename = Path.pack filename,
            rating = rating,
            rotate = orientation,
            crop = crop,
            gamma = gamma,
            zones = zones,
            blackpoint = blackpoint,
            whitepoint = whitepoint,
            expressions = expressions
          }
      )
