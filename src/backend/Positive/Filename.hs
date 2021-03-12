{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Positive.Filename where

import qualified Bound
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Generics.SOP as SOP
import qualified Language.Elm.Definition as Definition
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Pattern as Pattern
import qualified Language.Elm.Type as Type
import qualified Language.Haskell.To.Elm as Elm
import qualified Language.Haskell.To.Elm.Via as Elm
import Positive.Prelude

newtype Filename = Filename Text
  deriving (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, NFData, Hashable)
  deriving newtype (Aeson.ToJSONKey, Aeson.FromJSONKey)
  deriving
    ( Aeson.ToJSON,
      Aeson.FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType Filename

instance Elm.HasElmEncoder Text Filename where
  elmEncoderDefinition =
    Just $
      Definition.Constant
        "Generated.Data.filenameToString"
        1
        ( Bound.toScope $
            Bound.B
              <$> Type.Fun "Generated.Data.Filename" "Basics.String"
        )
        $ Expression.Lam $
          Bound.toScope $
            Expression.Case
              (pure $ Bound.B ())
              [ ( Pattern.Con "Generated.Data.Filename" [Pattern.Var 0],
                  Bound.toScope (pure $ Bound.B 0)
                )
              ]

toFilePath :: Filename -> FilePath
toFilePath (Filename x) = Text.unpack x

toByteString :: Filename -> ByteString
toByteString (Filename x) = encodeUtf8 x

toText :: Filename -> Text
toText (Filename x) = x

fromFilePath :: FilePath -> Filename
fromFilePath = Filename . Text.pack
