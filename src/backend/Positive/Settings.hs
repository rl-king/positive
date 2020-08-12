{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Positive.Settings where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import qualified Language.Haskell.To.Elm as Elm

data ImageSettings = ImageSettings
  { isPath :: Text,
    isGamma :: Double,
    isZone1 :: Double,
    isZone5 :: Double,
    isZone9 :: Double,
    isBlackpoint :: Double,
    isWhitepoint :: Double
  }
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Elm.HasElmType ImageSettings where
  elmDefinition =
    Just $
      Elm.deriveElmTypeDefinition
        @ImageSettings
        Elm.defaultOptions
        "Generated.Data.ImageSettings.ImageSettings"

instance Elm.HasElmDecoder Aeson.Value ImageSettings where
  elmDecoderDefinition =
    Just $
      Elm.deriveElmJSONDecoder
        @ImageSettings
        Elm.defaultOptions
        Aeson.defaultOptions
        "Generated.Data.ImageSettings.decodeCoordinate"

instance Elm.HasElmEncoder Aeson.Value ImageSettings where
  elmEncoderDefinition =
    Just $
      Elm.deriveElmJSONEncoder
        @ImageSettings
        Elm.defaultOptions
        Aeson.defaultOptions
        "Generated.Data.ImageSettings.encodeCoordinate"
