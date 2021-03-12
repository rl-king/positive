{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Positive.Image.Settings where

import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import qualified Generics.SOP as SOP
import qualified Language.Haskell.To.Elm as Elm
import qualified Language.Haskell.To.Elm.Via as Elm
import Positive.Filename
import Positive.Prelude
import Servant

-- SETTINGS

data Settings = Settings
  { iFilename :: !Filename,
    iRotate :: !Double,
    iCrop :: !ImageCrop,
    iGamma :: !Double,
    iZones :: !Zones,
    iBlackpoint :: !Double,
    iWhitepoint :: !Double,
    iExpressions :: !(Vector Expression)
  }
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, NFData)
  deriving
    ( Aeson.ToJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType Settings

instance Aeson.FromJSON Settings where
  parseJSON =
    Aeson.withObject "Settings" $ \o -> do
      filename <- o .: "iFilename"
      rotate <- o .: "iRotate"
      crop <- o .: "iCrop"
      gamma <- o .: "iGamma"
      zone1 <- o .:? "iZone1" .!= 0
      zone5 <- o .:? "iZone5" .!= 0
      zone9 <- o .:? "iZone9" .!= 0
      zones <- o .:? "iZones" .!= Zones zone1 0 0 0 zone5 0 0 0 zone9
      blackpoint <- o .: "iBlackpoint"
      whitepoint <- o .: "iWhitepoint"
      expressions <- o .:? "iExpressions" .!= mempty
      pure $
        Settings
          { iFilename = filename,
            iRotate = rotate,
            iCrop = crop,
            iGamma = gamma,
            iZones = zones,
            iBlackpoint = blackpoint,
            iWhitepoint = whitepoint,
            iExpressions = expressions
          }

instance FromHttpApiData Settings where
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
