{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Positive.Settings where

import qualified Data.Aeson as Aeson
import Data.Bifunctor
import qualified Data.ByteString.Base64 as Base64
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text as Text
import Data.Text.Encoding as Text
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import qualified Language.Haskell.To.Elm as Elm
import Servant

-- FILMROLLSETTINGS

newtype FilmRollSettings = FilmRollSettings
  { unFilmRollSettings :: HashMap Text ImageSettings
  }
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

init :: ImageSettings -> FilmRollSettings
init imageSettings =
  FilmRollSettings $ HashMap.insert (iFilename imageSettings) imageSettings mempty

insert :: ImageSettings -> FilmRollSettings -> FilmRollSettings
insert imageSettings =
  FilmRollSettings . HashMap.insert (iFilename imageSettings) imageSettings . unFilmRollSettings

fromList :: [Text] -> FilmRollSettings
fromList =
  FilmRollSettings
    . HashMap.fromList
    . fmap (\x -> (x, ImageSettings x 0 noCrop 2.2 0 0 0 0 0))

toList :: FilmRollSettings -> [ImageSettings]
toList =
  HashMap.elems . unFilmRollSettings

-- IMAGESETTINGS

data ImageSettings = ImageSettings
  { iFilename :: Text,
    iRotate :: Double,
    iCrop :: ImageCrop,
    iGamma :: Double,
    iZone1 :: Double,
    iZone5 :: Double,
    iZone9 :: Double,
    iBlackpoint :: Double,
    iWhitepoint :: Double
  }
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance FromHttpApiData ImageSettings where
  parseUrlPiece piece =
    first Text.pack $
      Aeson.eitherDecodeStrict =<< Base64.decode (Text.encodeUtf8 piece)
  parseQueryParam = parseUrlPiece

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
        "Generated.Data.ImageSettings.decodeImageSettings"

instance Elm.HasElmEncoder Aeson.Value ImageSettings where
  elmEncoderDefinition =
    Just $
      Elm.deriveElmJSONEncoder
        @ImageSettings
        Elm.defaultOptions
        Aeson.defaultOptions
        "Generated.Data.ImageSettings.encodeImageSettings"

-- CROP

data ImageCrop = ImageCrop
  { icTop :: Double,
    icLeft :: Double,
    icWidth :: Double
  }
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

noCrop :: ImageCrop
noCrop =
  ImageCrop 0 0 100

instance Elm.HasElmType ImageCrop where
  elmDefinition =
    Just $
      Elm.deriveElmTypeDefinition
        @ImageCrop
        Elm.defaultOptions
        "Generated.Data.ImageSettings.ImageCrop"

instance Elm.HasElmDecoder Aeson.Value ImageCrop where
  elmDecoderDefinition =
    Just $
      Elm.deriveElmJSONDecoder
        @ImageCrop
        Elm.defaultOptions
        Aeson.defaultOptions
        "Generated.Data.ImageSettings.decodeImageCrop"

instance Elm.HasElmEncoder Aeson.Value ImageCrop where
  elmEncoderDefinition =
    Just $
      Elm.deriveElmJSONEncoder
        @ImageCrop
        Elm.defaultOptions
        Aeson.defaultOptions
        "Generated.Data.ImageSettings.encodeImageCrop"
