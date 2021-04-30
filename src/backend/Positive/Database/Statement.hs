{-# LANGUAGE QuasiQuotes #-}

module Positive.Database.Statement where

import Data.Aeson as Aeson
import Hasql.Statement (Statement (..))
import Hasql.TH
import Positive.Prelude

insertImageSettings ::
  Statement
    ( Text,
      Int16,
      Double,
      Aeson.Value,
      Double,
      Aeson.Value,
      Double,
      Double,
      Aeson.Value,
      Int32
    )
    Int32
insertImageSettings =
  [singletonStatement|
    insert into positive.image (filename, rating, orientation, crop, gamma,
    zones, blackpoint, whitepoint, expressions, filmroll_id)
    values ($1 :: text, $2 :: int2, $3 :: float8, $4 :: jsonb, $5 :: float8,
    $6 :: jsonb, $7 :: float8 , $8 :: float8, $9 :: jsonb, $10 :: int4)
    returning id :: int4
  |]

insertFilmRoll :: Statement (Text, Maybe Int32) Int32
insertFilmRoll =
  [singletonStatement|
    insert into positive.filmroll (directory_path, poster)
    values ($1 :: text, $2 :: int4?)
    returning id :: int4
  |]
