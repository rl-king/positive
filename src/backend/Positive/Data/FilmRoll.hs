{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Positive.Data.FilmRoll where

import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import qualified Generics.SOP as SOP
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Type as Type
import qualified Language.Haskell.To.Elm as Elm
import qualified Language.Haskell.To.Elm.Via as Elm
import Positive.Data.Filename
import Positive.Data.Id
import Positive.Data.ImageSettings
import Positive.Prelude

-- FILMROLLSETTINGS

data FilmRoll = FilmRoll
  { filmRollId :: FilmRollId,
    directoryPath :: Text,
    poster :: Maybe Filename,
    imageSettings :: HashMap Filename ImageSettings
  }
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, NFData)
  deriving
    ( ToJSON,
      FromJSON,
      Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType FilmRoll

empty :: FilmRollId -> FilmRoll
empty id =
  FilmRoll id "" Nothing mempty

-- isEmpty :: FilmRoll -> Bool
-- isEmpty =
--   error "still needed?"

-- -- (==) empty

-- init :: ImageSettings -> FilmRoll
-- init imageSettings =
--   error "still needed?"

-- -- empty{imageSettings =
-- --         HashMap.insert imageSettings.filename imageSettings mempty
-- --      }

-- insert :: ImageSettings -> FilmRoll -> FilmRoll
-- insert imageSettings filmRollSettings =
--   filmRollSettings{imageSettings =
--                      HashMap.insert
--                        imageSettings.filename
--                        imageSettings
--                        filmRollSettings.imageSettings
--                   }

-- fromList :: [ImageSettings] -> FilmRoll
-- fromList settings =
--   empty{imageSettings =
--           HashMap.fromList $
--             fmap (\is -> (is.filename, is)) settings
--        }

-- fromFilenames :: [Filename] -> FilmRoll
-- fromFilenames xs =
--   empty{imageSettings =
--           HashMap.fromList $
--             fmap (\x -> (x, plainImageSettings x)) xs
--        }

toList :: FilmRoll -> [ImageSettings]
toList =
  HashMap.elems . imageSettings

-- difference :: FilmRoll -> FilmRoll -> FilmRoll
-- difference (FilmRoll pa sa a) (FilmRoll pb sb b) =
--   FilmRoll
--     (pa <|> pb)
--     (sa <> sb)
--     (HashMap.differenceWith (\x y -> if x /= y then Just x else Nothing) a b)

-- plainImageSettings :: Filename -> ImageSettings
-- plainImageSettings x =
--   ImageSettings x 0 noCrop 2.2 initZones 0 1 mempty

instance Elm.HasElmType a => Elm.HasElmType (HashMap Filename a) where
  elmType =
    Type.apps
      "Dict.Fun.Dict"
      ["Generated.Data.Filename", "Basics.String", Elm.elmType @a]

instance
  Elm.HasElmEncoder Aeson.Value a =>
  Elm.HasElmEncoder Aeson.Value (HashMap Filename a)
  where
  elmEncoder =
    Expression.App "Dict.Fun.encode" (Elm.elmEncoder @Aeson.Value @a)

instance
  Elm.HasElmDecoder Aeson.Value a =>
  Elm.HasElmDecoder Aeson.Value (HashMap Filename a)
  where
  elmDecoder =
    Expression.apps
      "Dict.Fun.decoder"
      [ "Generated.Data.filenameToString",
        "Generated.Data.Filename",
        Elm.elmDecoder @Aeson.Value @a
      ]
