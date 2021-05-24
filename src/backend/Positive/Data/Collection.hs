{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Data.Collection where

import qualified Data.Aeson as Aeson
import Data.Maybe
import qualified Generics.SOP as SOP
import qualified Language.Haskell.To.Elm as Elm
import qualified Language.Haskell.To.Elm.Via as Elm
import Positive.Data.HKD
import Positive.Data.Id
import Positive.Prelude

-- ALIAS

type Collection = CollectionBase FromDatabase Identity

type NewCollection = CollectionBase New Maybe

-- FILMROLL

data CollectionBase t f = CollectionBase
  { id :: P t f CollectionId,
    title :: Text,
    created :: P t f UTCTime,
    modified :: P t f UTCTime,
    target :: Bool,
    imageIds :: [ImageSettingsId]
  }
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)

deriving via Elm.ElmType "Collection" Collection instance Aeson.ToJSON Collection

deriving via Elm.ElmType "Collection" Collection instance Aeson.FromJSON Collection

deriving via Elm.ElmType "Collection" Collection instance Elm.HasElmType Collection

deriving via Elm.ElmType "Collection" Collection instance Elm.HasElmDecoder Aeson.Value Collection

deriving via Elm.ElmType "Collection" Collection instance Elm.HasElmEncoder Aeson.Value Collection

deriving instance Show Collection

deriving instance NFData Collection

emptyCollection :: Text -> NewCollection
emptyCollection title =
  CollectionBase
    { id = Nothing,
      created = Nothing,
      modified = Nothing,
      target = False,
      title = title,
      imageIds = mempty
    }
