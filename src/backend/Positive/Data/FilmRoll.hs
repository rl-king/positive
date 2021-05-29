{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Data.FilmRoll where

import qualified Data.Aeson as Aeson
import Data.Maybe
import qualified Data.Time.Calendar as Calendar
import qualified Generics.SOP as SOP
import qualified Language.Haskell.To.Elm as Elm
import qualified Language.Haskell.To.Elm.Via as Elm
import Positive.Data.HKD
import Positive.Data.Id
import Positive.Data.ImageSettings
import Positive.Data.Path
import Positive.Prelude

-- ALIAS

type FilmRoll = FilmRollBase FromDatabase Identity

type NewFilmRoll = FilmRollBase New Maybe

-- FILMROLL

data FilmRollBase t f = FilmRollBase
  { id :: P t f FilmRollId,
    poster :: Maybe ImageSettingsId,
    directoryPath :: Directory,
    created :: P t f UTCTime,
    modified :: P t f UTCTime,
    developedOn :: P t f DevelopedOn,
    imageSettings :: [ImageSettings]
  }
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)

deriving via Elm.ElmType "FilmRoll" FilmRoll instance Aeson.ToJSON FilmRoll

deriving via Elm.ElmType "FilmRoll" FilmRoll instance Aeson.FromJSON FilmRoll

deriving via Elm.ElmType "FilmRoll" FilmRoll instance Elm.HasElmType FilmRoll

deriving via Elm.ElmType "FilmRoll" FilmRoll instance Elm.HasElmDecoder Aeson.Value FilmRoll

deriving via Elm.ElmType "FilmRoll" FilmRoll instance Elm.HasElmEncoder Aeson.Value FilmRoll

instance Elm.HasElmType DevelopedOn where
  elmType = "Date.Date"

instance Elm.HasElmDecoder Aeson.Value DevelopedOn where
  elmDecoder = "Util.decodeDate"

instance Elm.HasElmEncoder Aeson.Value DevelopedOn where
  elmEncoder = "Util.encodeDate"

deriving instance Show FilmRoll

deriving instance NFData FilmRoll

emptyFilmRoll :: Directory -> NewFilmRoll
emptyFilmRoll directoryPath =
  FilmRollBase
    { id = Nothing,
      created = Nothing,
      modified = Nothing,
      poster = Nothing,
      directoryPath = directoryPath,
      developedOn = Nothing,
      imageSettings = []
    }

newtype DevelopedOn = DevelopedOn
  { unDevelopedOn :: Day
  }
  deriving (Show, Generic)
  deriving anyclass (NFData, SOP.Generic, SOP.HasDatatypeInfo)
  deriving newtype (ToJSON, FromJSON)
