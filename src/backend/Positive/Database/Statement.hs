{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Positive.Database.Statement where

import Data.Aeson as Aeson
import Hasql.Decoders as Decode
import Hasql.Encoders as Encode
import Hasql.Statement (Statement (..))
import Hasql.TH
import Positive.Prelude

-- INSERT

insertImageSettings :: Statement _ (Int32, Text)
insertImageSettings =
  [singletonStatement|
    insert into positive.image (filename, rating, orientation, crop, gamma,
    zones, blackpoint, whitepoint, expressions, film_roll_id)
    values ($1 :: text, $2 :: int2, $3 :: float8, $4 :: jsonb, $5 :: float8,
    $6 :: jsonb, $7 :: float8 , $8 :: float8, $9 :: jsonb, $10 :: int4)
    returning id :: int4, filename :: text
  |]

insertFilmRoll :: Statement Text Int32
insertFilmRoll =
  [singletonStatement|
    insert into positive.film_roll (directory_path)
    values ($1 :: text)
    returning id :: int4
  |]

-- UPDATE

updatePoster :: Statement (Maybe Int32, Int32) Int32
updatePoster =
  [singletonStatement|
    update positive.film_roll set poster = $1 :: int4? where id = $2 :: int4
    returning id :: int4
  |]

-- SELECT

selectFilmRolls :: Statement () (Vector _)
selectFilmRolls =
  [vectorStatement|
    select film_roll.id :: int4, directory_path :: text, poster :: text?,
    image.id :: int4, filename :: text, rating :: int2, orientation :: float8,
    crop :: jsonb, gamma :: float8, zones :: jsonb, blackpoint :: float8,
    whitepoint :: float8, expressions :: jsonb
    from positive.film_roll
    join positive.image on film_roll.id = image.film_roll_id
  |]
