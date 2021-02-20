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
import Data.String (fromString)
import qualified Generics.SOP as SOP
import qualified Language.Elm.Name as Name
import Language.Haskell.To.Elm
import Positive.Prelude

newtype ElmType a
  = ElmType a

instance (Generic a, Aeson.GToJSON Aeson.Zero (Rep a)) => Aeson.ToJSON (ElmType a) where
  toJSON (ElmType a) =
    Aeson.genericToJSON Aeson.defaultOptions a

instance (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) => Aeson.FromJSON (ElmType a) where
  parseJSON =
    fmap ElmType . Aeson.genericParseJSON Aeson.defaultOptions

instance (SOP.HasDatatypeInfo a, SOP.All2 HasElmType (SOP.Code a)) => HasElmType (ElmType a) where
  elmDefinition =
    Just
      . deriveElmTypeDefinition @a defaultOptions
      . fromString
      . ("Generated.Data." <>)
      . SOP.datatypeName
      . SOP.datatypeInfo
      $ Proxy @a

instance
  ( SOP.HasDatatypeInfo a,
    HasElmType a,
    SOP.All2 (HasElmDecoder Aeson.Value) (SOP.Code a),
    HasElmType (ElmType a)
  ) =>
  HasElmDecoder Aeson.Value (ElmType a)
  where
  elmDecoderDefinition =
    Just
      . deriveElmJSONDecoder @a defaultOptions Aeson.defaultOptions
      . Name.Qualified ["Generated", "Data"]
      . fromString
      $ "decode" <> SOP.datatypeName (SOP.datatypeInfo (Proxy @a))

instance
  ( SOP.HasDatatypeInfo a,
    HasElmType a,
    SOP.All2 (HasElmEncoder Aeson.Value) (SOP.Code a),
    HasElmType (ElmType a)
  ) =>
  HasElmEncoder Aeson.Value (ElmType a)
  where
  elmEncoderDefinition =
    Just
      . deriveElmJSONEncoder @a defaultOptions Aeson.defaultOptions
      . Name.Qualified ["Generated", "Data"]
      . fromString
      $ "encode" <> SOP.datatypeName (SOP.datatypeInfo (Proxy @a))
