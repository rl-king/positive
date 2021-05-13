{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Positive.Data.ImageSettings where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import qualified Generics.SOP as SOP
import qualified Language.Haskell.To.Elm as Elm
import qualified Language.Haskell.To.Elm.Via as Elm
import Positive.Data.HKD
import Positive.Data.Id (FilmRollId, ImageSettingsId)
import Positive.Data.Path
import Positive.Prelude
import Servant

-- ALIAS

type ImageSettings = ImageSettingsBase FromDatabase Identity

type NewImageSettings = ImageSettingsBase New Maybe

-- SETTINGS

data ImageSettingsBase t f = ImageSettingsBase
  { id :: Unwrap t f ImageSettingsId,
    filmRollId :: FilmRollId,
    filename :: Filename,
    rating :: Int16,
    rotate :: Double,
    crop :: ImageCrop,
    gamma :: Double,
    zones :: Zones,
    blackpoint :: Double,
    whitepoint :: Double,
    expressions :: Vector Expression,
    histogram :: Vector Word8
  }
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)

deriving via Elm.ElmType "ImageSettings" ImageSettings instance Aeson.ToJSON ImageSettings

deriving via Elm.ElmType "ImageSettings" ImageSettings instance Aeson.FromJSON ImageSettings

deriving via Elm.ElmType "ImageSettings" ImageSettings instance Elm.HasElmType ImageSettings

deriving via Elm.ElmType "ImageSettings" ImageSettings instance Elm.HasElmDecoder Aeson.Value ImageSettings

deriving via Elm.ElmType "ImageSettings" ImageSettings instance Elm.HasElmEncoder Aeson.Value ImageSettings

deriving instance Show ImageSettings

deriving instance NFData ImageSettings

instance FromHttpApiData ImageSettings where
  parseUrlPiece piece =
    first Text.pack $
      Aeson.eitherDecodeStrict =<< Base64.decode (encodeUtf8 piece)
  parseQueryParam = parseUrlPiece

emptyImageSettings :: FilmRollId -> Filename -> NewImageSettings
emptyImageSettings filmRollId filename =
  ImageSettingsBase
    { id = Nothing,
      filmRollId = filmRollId,
      filename = filename,
      rating = 0,
      rotate = 0,
      crop = emptyCrop,
      gamma = 2.2,
      zones = emptyZones,
      blackpoint = 0,
      whitepoint = 1,
      expressions = mempty,
      histogram = mempty
    }

-- Zones

data Zones = Zones
  { z1 :: Double,
    z2 :: Double,
    z3 :: Double,
    z4 :: Double,
    z5 :: Double,
    z6 :: Double,
    z7 :: Double,
    z8 :: Double,
    z9 :: Double
  }
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, NFData)
  deriving
    ( Aeson.ToJSON,
      Aeson.FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType "Zones" Zones

emptyZones :: Zones
emptyZones =
  Zones 0 0 0 0 0 0 0 0 0

-- EXPR

data Expression = Expression
  { eValue :: Double,
    eMin :: Double,
    eMax :: Double,
    eLabel :: Text,
    eExpr :: Text
  }
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, NFData)
  deriving
    ( Aeson.ToJSON,
      Aeson.FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType "Expression" Expression

emptyExpression :: Expression
emptyExpression =
  Expression 0 (-1) 1 "" ""

-- EXPR RESULT

data ExpressionResult
  = SyntaxError Text
  | TypeError Text
  | SampleEval [Double]
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, NFData)
  deriving
    ( Aeson.ToJSON,
      Aeson.FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType "ExpressionResult" ExpressionResult

-- CROP

data ImageCrop = ImageCrop
  { icTop :: Double,
    icLeft :: Double,
    icWidth :: Double
  }
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, NFData)
  deriving
    ( Aeson.ToJSON,
      Aeson.FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType "ImageCrop" ImageCrop

emptyCrop :: ImageCrop
emptyCrop =
  ImageCrop 0 0 100

-- COORDINATE

data CoordinateInfo = CoordinateInfo
  { ciX :: Double,
    ciY :: Double,
    ciValue :: Double
  }
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    ( Aeson.ToJSON,
      Aeson.FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType "CoordinateInfo" CoordinateInfo
