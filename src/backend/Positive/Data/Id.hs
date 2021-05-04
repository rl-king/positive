{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}

module Positive.Data.Id
  ( pack,
    unpack,
    Id,
    ImageSettingsId,
    FilmRollId,
  )
where

import qualified Data.Aeson as Aeson
import qualified Generics.SOP as SOP
import qualified Language.Haskell.To.Elm as Elm
import Positive.Prelude hiding (pack)
import Servant

newtype Id (a :: Symbol)
  = Id Int32
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, NFData)
  deriving (ToHttpApiData, FromHttpApiData) via Int32
  deriving
    ( Aeson.ToJSON,
      Aeson.FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value,
      Elm.HasElmEncoder Text
    )
    via Int32

type FilmRollId =
  Id "FilmRoll"

type ImageSettingsId =
  Id "ImageSettings"

unpack :: Id a -> Int32
unpack (Id id) = id

pack :: Int32 -> Id a
pack = Id
