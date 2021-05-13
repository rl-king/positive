{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Data.FilmRoll where

import qualified Data.Aeson as Aeson
import Data.Maybe
import qualified Generics.SOP as SOP
import qualified Language.Haskell.To.Elm as Elm
import qualified Language.Haskell.To.Elm.Via as Elm
import Positive.Data.Id
import Positive.Data.ImageSettings
import Positive.Data.Path
import Positive.Prelude

-- FILMROLL

data FilmRoll = FilmRoll
  { id :: FilmRollId,
    poster :: Maybe ImageSettingsId,
    directoryPath :: Directory,
    imageSettings :: [ImageSettings]
  }
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, NFData)
  deriving
    ( Aeson.ToJSON,
      Aeson.FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType FilmRoll

empty :: FilmRollId -> FilmRoll
empty id =
  FilmRoll id Nothing "" mempty
