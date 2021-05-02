{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Positive.Data.ImageSettings where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import qualified Generics.SOP as SOP
import qualified Language.Haskell.To.Elm as Elm
import qualified Language.Haskell.To.Elm.Via as Elm
import Positive.Data.Filename
import Positive.Prelude
import Servant

-- SETTINGS

data ImageSettings = ImageSettings
  { filename :: !Filename,
    rating :: !Int16,
    rotate :: !Double,
    crop :: !ImageCrop,
    gamma :: !Double,
    zones :: !Zones,
    blackpoint :: !Double,
    whitepoint :: !Double,
    expressions :: !(Vector Expression)
  }
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, NFData)
  deriving
    ( Aeson.ToJSON,
      Aeson.FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType ImageSettings

instance FromHttpApiData ImageSettings where
  parseUrlPiece piece =
    first Text.pack $
      Aeson.eitherDecodeStrict =<< Base64.decode (encodeUtf8 piece)
  parseQueryParam = parseUrlPiece

-- Zones

data Zones = Zones
  { z1 :: !Double,
    z2 :: !Double,
    z3 :: !Double,
    z4 :: !Double,
    z5 :: !Double,
    z6 :: !Double,
    z7 :: !Double,
    z8 :: !Double,
    z9 :: !Double
  }
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, NFData)
  deriving
    ( Aeson.ToJSON,
      Aeson.FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType Zones

initZones :: Zones
initZones =
  Zones 0 0 0 0 0 0 0 0 0

-- EXPR

data Expression = Expression
  { eValue :: !Double,
    eMin :: !Double,
    eMax :: !Double,
    eLabel :: !Text,
    eExpr :: !Text
  }
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, NFData)
  deriving
    ( Aeson.ToJSON,
      Aeson.FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType Expression

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
    via Elm.ElmType ExpressionResult

-- CROP

data ImageCrop = ImageCrop
  { icTop :: !Double,
    icLeft :: !Double,
    icWidth :: !Double
  }
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, NFData)
  deriving
    ( Aeson.ToJSON,
      Aeson.FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType ImageCrop

noCrop :: ImageCrop
noCrop =
  ImageCrop 0 0 100

-- COORDINATE

data CoordinateInfo = CoordinateInfo
  { ciX :: !Double,
    ciY :: !Double,
    ciValue :: !Double
  }
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    ( Aeson.ToJSON,
      Aeson.FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType CoordinateInfo
