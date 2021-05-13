{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.To.Elm.Via where

import qualified Data.Aeson as Aeson
import Data.Proxy
import qualified Generics.SOP as SOP
import qualified Language.Elm.Name as Name
import Language.Haskell.To.Elm
import Positive.Prelude

newtype ElmType (name :: Symbol) a
  = ElmType a

instance
  (Generic a, Aeson.GToJSON Aeson.Zero (Rep a)) =>
  Aeson.ToJSON (ElmType name a)
  where
  toJSON (ElmType a) =
    Aeson.genericToJSON Aeson.defaultOptions a

instance
  (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) =>
  Aeson.FromJSON (ElmType name a)
  where
  parseJSON =
    fmap ElmType . Aeson.genericParseJSON Aeson.defaultOptions

instance
  ( SOP.HasDatatypeInfo a,
    SOP.All2 HasElmType (SOP.Code a),
    KnownSymbol name
  ) =>
  HasElmType (ElmType name a)
  where
  elmDefinition =
    Just
      . deriveElmTypeDefinition @a defaultOptions
      . fromString
      . ("Generated.Data." <>)
      . symbolVal
      $ Proxy @name

instance
  ( SOP.HasDatatypeInfo a,
    HasElmType a,
    SOP.All2 (HasElmDecoder Aeson.Value) (SOP.Code a),
    HasElmType (ElmType name a),
    KnownSymbol name
  ) =>
  HasElmDecoder Aeson.Value (ElmType name a)
  where
  elmDecoderDefinition =
    Just
      . deriveElmJSONDecoder @a defaultOptions Aeson.defaultOptions
      . Name.Qualified ["Generated", "Data"]
      . fromString
      . ("decode" <>)
      . symbolVal
      $ Proxy @name

instance
  ( SOP.HasDatatypeInfo a,
    HasElmType a,
    SOP.All2 (HasElmEncoder Aeson.Value) (SOP.Code a),
    HasElmType (ElmType name a),
    KnownSymbol name
  ) =>
  HasElmEncoder Aeson.Value (ElmType name a)
  where
  elmEncoderDefinition =
    Just
      . deriveElmJSONEncoder @a defaultOptions Aeson.defaultOptions
      . Name.Qualified ["Generated", "Data"]
      . fromString
      . ("encode" <>)
      . symbolVal
      $ Proxy @name
