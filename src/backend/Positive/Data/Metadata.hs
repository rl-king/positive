{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Data.Metadata where

import Data.Maybe
import Positive.Data.HKD
import Positive.Data.Id
import Positive.Prelude


-- ALIAS

type Metadata = MetadataBase FromDatabase Identity


type UpsertMetadata = MetadataBase New Maybe


-- METADATA

data MetadataBase t f = MetadataBase
  { id :: P t f MetadataId
  , imageId :: ImageSettingsId
  , preview_updated :: Maybe UTCTime
  , histogram :: Vector Int
  }


deriving instance Show Metadata
