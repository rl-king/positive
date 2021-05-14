{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Positive.Database.Session where

import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Hasql.Connection
import qualified Hasql.Decoders as Decode
import qualified Hasql.Encoders as Encode
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction as Transaction
import Positive.Data.FilmRoll as FilmRoll
import Positive.Data.Id
import qualified Positive.Data.Id as Id
import Positive.Data.ImageSettings as ImageSettings
import Positive.Data.Metadata as Metadata
import qualified Positive.Data.Path as Path
import Positive.Prelude

-- INSERT

insertImageSettings :: FilmRollId -> Path.Filename -> Session (ImageSettingsId, Text)
insertImageSettings filmRollId filename =
  let sql =
        "insert into positive.image\
        \ (film_roll_id, filename, rating, orientation, crop, gamma,\
        \ zones, blackpoint, whitepoint, expressions)\
        \ values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)\
        \ returning id :: int4, filename :: text"
      decoder =
        Decode.singleRow $
          (\a b -> (Id.pack a, b))
            <$> column Decode.int4
            <*> column Decode.text
   in Session.statement (emptyImageSettings filmRollId filename) $
        Statement sql encodeNewImageSettings decoder True

insertFilmRoll :: Path.Directory -> Session FilmRollId
insertFilmRoll directory_path =
  let sql =
        "insert into positive.film_roll\
        \ (poster, directory_path)\
        \ values ($1, $2)\
        \ returning id"
      decoder =
        Decode.singleRow $ Id.pack <$> column Decode.int4
   in Session.statement (emptyFilmRoll directory_path) $
        Statement sql encodeNewFilmRoll decoder True

-- UPDATE

updateImageSettingsList :: [ImageSettings] -> Transaction ()
updateImageSettingsList =
  traverse_ updateImageSettings

updateImageSettings :: ImageSettings -> Transaction ()
updateImageSettings imageSettings =
  let sql =
        "update positive.image set\
        \ film_roll_id = $2,\
        \ filename = $3,\
        \ rating = $4,\
        \ orientation = $5,\
        \ crop = $6,\
        \ gamma = $7,\
        \ zones = $8,\
        \ blackpoint = $9,\
        \ whitepoint = $10,\
        \ expressions = $11\
        \ where id = $1"
   in Transaction.statement imageSettings $
        Statement sql encodeImageSettings Decode.noResult True

updateMetadata :: UpdateMetadata -> Session ()
updateMetadata newMetadata =
  let sql =
        "insert into positive.image_metadata\
        \ (image_id, histogram, preview_updated)\
        \ values($1, $2, now())\
        \ on conflict (image_id)\
        \ do update set preview_updated = now(), histogram = $2"
   in Session.statement newMetadata $
        Statement sql encodeUpdateMetadata Decode.noResult True

updatePoster :: Maybe ImageSettingsId -> FilmRollId -> Session ()
updatePoster imageSettingsId filmRollId =
  let sql = "update positive.film_roll set poster = $1 where id = $2"
      encoder =
        (fmap Id.unpack . fst >$< nullableParam Encode.int4)
          <> (Id.unpack . snd >$< param Encode.int4)
   in Session.statement (imageSettingsId, filmRollId) $
        Statement sql encoder Decode.noResult True

-- SELECT

selectOutdatedPreviews :: Session [(Path.Directory, ImageSettings)]
selectOutdatedPreviews =
  let sql =
        "select directory_path, image.*, histogram from positive.image\
        \ join positive.film_roll on film_roll.id = image.film_roll_id\
        \ left join positive.image_metadata on image_metadata.image_id = image.id\
        \ where image.modified > preview_updated\
        \ order by film_roll_id, image.modified"
      decoder =
        Decode.rowList $
          (,) <$> (Path.pack <$> column Decode.text) <*> decodeImageSettings
   in Session.statement () $
        Statement sql Encode.noParams decoder True

selectImageSettingsByPath ::
  Path.Directory ->
  Path.Filename ->
  Session (Path.Directory, ImageSettings)
selectImageSettingsByPath dir filename =
  let sql =
        "select directory_path, image.*, histogram from positive.film_roll\
        \ join positive.image\
        \ join positive.image_metadata on image_metadata.image_id = image.id\
        \ on film_roll.id = image.film_roll_id and $2 = image.filename\
        \ where directory_path = $1"
      encoder =
        (Path.unpack . fst >$< param Encode.text)
          <> (Path.unpack . snd >$< param Encode.text)
      decoder =
        Decode.singleRow $
          (,) <$> (Path.pack <$> column Decode.text) <*> decodeImageSettings
   in Session.statement (dir, filename) $
        Statement sql encoder decoder True

selectDirectoryPath :: ImageSettingsId -> Session Path.Directory
selectDirectoryPath imageSettingsId =
  let sql =
        "select directory_path from positive.film_roll\
        \ join positive.image on film_roll.id = image.film_roll_id\
        \ where image.id = $1"
   in Session.statement imageSettingsId $
        Statement
          sql
          (param $ Id.unpack >$< Encode.int4)
          (Decode.singleRow $ column (Path.pack <$> Decode.text))
          True

selectFilmRolls :: Session [FilmRoll]
selectFilmRolls =
  let sql =
        "select film_roll.id, film_roll.poster, film_roll.directory_path,\
        \ image.*, histogram from positive.film_roll\
        \ join positive.image on film_roll.id = image.film_roll_id\
        \ left join positive.image_metadata on image_metadata.image_id = image.id"
      merge newFilmRoll acc =
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
   in Session.statement () $
        Statement
          sql
          Encode.noParams
          (HashMap.elems <$> Decode.foldrRows merge mempty decodeFilmRoll)
          True

selectFilmRollByImageSettings :: ImageSettingsId -> Session FilmRoll
selectFilmRollByImageSettings imageSettingsId =
  let sql =
        "select film_roll.id, film_roll.poster, film_roll.directory_path,\
        \ image.*, histogram from positive.film_roll\
        \ join positive.image on film_roll.id = image.film_roll_id\
        \ join positive.image_metadata on image_metadata.image_id = image.id\
        \ where image.id = $1"
   in Session.statement imageSettingsId $
        Statement
          sql
          (param $ Id.unpack >$< Encode.int4)
          (Decode.singleRow decodeFilmRoll)
          True

-- EN-DECODEING

encodeNewFilmRoll :: Encode.Params NewFilmRoll
encodeNewFilmRoll =
  mconcat
    [ fmap Id.unpack . poster >$< nullableParam Encode.int4,
      Path.unpack . directoryPath >$< param Encode.text
    ]

encodeImageSettings :: Encode.Params ImageSettings
encodeImageSettings =
  mconcat
    [ Id.unpack . ImageSettings.id >$< param Encode.int4,
      encodeNewImageSettings
    ]

encodeNewImageSettings :: Encode.Params (ImageSettingsBase a b)
encodeNewImageSettings =
  mconcat
    [ Id.unpack . ImageSettings.filmRollId >$< param Encode.int4,
      Path.unpack . filename >$< param Encode.text,
      rating >$< param Encode.int2,
      rotate >$< param Encode.float8,
      Aeson.toJSON . crop >$< param Encode.jsonb,
      gamma >$< param Encode.float8,
      Aeson.toJSON . zones >$< param Encode.jsonb,
      blackpoint >$< param Encode.float8,
      whitepoint >$< param Encode.float8,
      Aeson.toJSON . expressions >$< param Encode.jsonb
    ]

encodeUpdateMetadata :: Encode.Params UpdateMetadata
encodeUpdateMetadata =
  mconcat
    [ Id.unpack . Metadata.imageId >$< param Encode.int4,
      fmap (toEnum . fromIntegral) . Metadata.histogram
        >$< param (Encode.foldableArray (Encode.nonNullable Encode.int4))
    ]

decodeFilmRoll :: Decode.Row FilmRoll
decodeFilmRoll =
  FilmRollBase
    <$> column (Id.pack <$> Decode.int4)
    <*> nullableColumn (Id.pack <$> Decode.int4)
    <*> column (Path.pack <$> Decode.text)
    <*> fmap pure decodeImageSettings

decodeImageSettings :: Decode.Row ImageSettings
decodeImageSettings =
  ImageSettingsBase
    <$> column (Id.pack <$> Decode.int4)
    <*> column (Path.pack <$> Decode.text)
    <*> column Decode.int2
    <*> column Decode.float8
    <*> column jsonb
    <*> column Decode.float8
    <*> column jsonb
    <*> column Decode.float8
    <*> column Decode.float8
    <*> column jsonb
    <*> column Decode.timestamptz
    <*> column Decode.timestamptz
    <*> column (Id.pack <$> Decode.int4)
    <*> ( fromMaybe mempty
            <$> nullableColumn
              ( fmap fromIntegral
                  <$> Decode.vectorArray (Decode.nonNullable Decode.int4)
              )
        )

decodeMetadata :: Decode.Row Metadata
decodeMetadata =
  MetadataBase
    <$> column (Id.pack <$> Decode.int4)
    <*> column (Id.pack <$> Decode.int4)
    <*> nullableColumn Decode.timestamptz
    <*> column
      ( fmap fromIntegral
          <$> Decode.vectorArray (Decode.nonNullable Decode.int4)
      )

param :: Encode.Value a -> Encode.Params a
param =
  Encode.param . Encode.nonNullable

nullableParam :: Encode.Value a -> Encode.Params (Maybe a)
nullableParam =
  Encode.param . Encode.nullable

column :: Decode.Value a -> Decode.Row a
column =
  Decode.column . Decode.nonNullable

nullableColumn :: Decode.Value a -> Decode.Row (Maybe a)
nullableColumn =
  Decode.column . Decode.nullable

jsonb :: FromJSON a => Decode.Value a
jsonb =
  Decode.refine
    (first Text.pack . Aeson.parseEither Aeson.parseJSON)
    Decode.jsonb

-- HELPER

rundb :: Session a -> IO (Either Session.QueryError a)
rundb session = do
  Right conn <-
    Hasql.Connection.acquire "host=localhost port=5432 dbname=positive"
  Session.run session conn
