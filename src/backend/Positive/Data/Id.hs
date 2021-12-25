{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Positive.Data.Id (
  pack,
  unpack,
  Id,
  ImageSettingsId,
  FilmRollId,
  MetadataId,
  CollectionId,
) where

import qualified Data.Aeson as Aeson
import qualified Language.Haskell.To.Elm as Elm
import Positive.Prelude hiding (pack)
import Servant


newtype Id (a :: Symbol)
  = Id Int32
  deriving (Eq, Generic, NFData, Hashable)
  deriving
    ( ToHttpApiData
    , FromHttpApiData
    , Aeson.ToJSON
    , Aeson.FromJSON
    )
    via Int32


instance KnownSymbol a => Show (Id a) where
  show (Id id) = symbolVal (Proxy @a) <> " " <> show id


deriving via Int instance Elm.HasElmType (Identity (Id a))


deriving via Int instance Elm.HasElmDecoder Aeson.Value (Identity (Id a))


deriving via Int instance Elm.HasElmEncoder Aeson.Value (Identity (Id a))


instance KnownSymbol a => Elm.HasElmType (Id a) where
  elmType =
    fromString $ "Data.Id." <> symbolVal (Proxy @a)


instance KnownSymbol a => Elm.HasElmDecoder Aeson.Value (Id a) where
  elmDecoder =
    "Data.Id.fromJson"


instance KnownSymbol a => Elm.HasElmEncoder Aeson.Value (Id a) where
  elmEncoder =
    "Data.Id.toJson"


instance KnownSymbol a => Elm.HasElmEncoder Text (Id a) where
  elmEncoder =
    "Data.Id.toString"


type FilmRollId =
  Id "FilmRollId"


type ImageSettingsId =
  Id "ImageSettingsId"


type MetadataId =
  Id "MetadataId"


type CollectionId =
  Id "CollectionId"


unpack :: Id a -> Int32
unpack (Id id) = id


pack :: Int32 -> Id a
pack = Id
