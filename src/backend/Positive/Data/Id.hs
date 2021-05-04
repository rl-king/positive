{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Positive.Data.Id
  ( pack,
    unpack,
    Id,
    ImageSettingsId,
    FilmRollId,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.String as String
import qualified Language.Haskell.To.Elm as Elm
import Positive.Prelude hiding (pack)
import Servant

newtype Id (a :: Symbol)
  = Id Int32
  deriving (Show, Eq, Generic, NFData)
  deriving
    ( ToHttpApiData,
      FromHttpApiData,
      Aeson.ToJSON,
      Aeson.FromJSON
    )
    via Int32

instance KnownSymbol a => Elm.HasElmType (Id a) where
  elmType =
    String.fromString $ "Data.Id." <> symbolVal (Proxy @a)

instance KnownSymbol a => Elm.HasElmDecoder Aeson.Value (Id a) where
  elmDecoder =
    "Data.Id.fromJson"

instance KnownSymbol a => Elm.HasElmEncoder Aeson.Value (Id a) where
  elmEncoder =
    "Data.Id.toJson"

type FilmRollId =
  Id "FilmRollId"

type ImageSettingsId =
  Id "ImageSettingsId"

unpack :: Id a -> Int32
unpack (Id id) = id

pack :: Int32 -> Id a
pack = Id
