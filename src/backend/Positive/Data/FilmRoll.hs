{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Positive.Data.ImageSettings
import Positive.Prelude

-- FILMROLLSETTINGS

data FilmRoll = FilmRoll
  { frsPoster :: !(Maybe Filename),
    frsRatings :: !(HashMap Filename Int),
    frsSettings :: !(HashMap Filename ImageSettings)
  }
  deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, NFData)
  deriving
    ( Elm.HasElmType,
      Elm.HasElmDecoder Aeson.Value,
      Elm.HasElmEncoder Aeson.Value
    )
    via Elm.ElmType FilmRoll

instance Semigroup FilmRoll where
  (<>) a b =
    FilmRoll
      { frsPoster = b.frsPoster <|> a.frsPoster,
        frsRatings = a.frsRatings <> b.frsRatings,
        frsSettings = a.frsSettings <> b.frsSettings
      }

instance Aeson.FromJSON FilmRoll where
  parseJSON =
    Aeson.withObject "FilmRoll" $ \o ->
      FilmRoll
        <$> o .:? "frsPoster"
        <*> o .:? "frsRatings" .!= mempty
        <*> (o .: "unFilmRollSettings" <|> o .: "frsSettings")

instance Aeson.ToJSON FilmRoll where
  toJSON filmRollSettings =
    Aeson.object
      [ "frsPoster" .= filmRollSettings.frsPoster,
        "frsSettings" .= filmRollSettings.frsSettings,
        "frsRatings" .= filmRollSettings.frsRatings
      ]

empty :: FilmRoll
empty =
  FilmRoll Nothing mempty mempty

isEmpty :: FilmRoll -> Bool
isEmpty =
  (==) empty

init :: ImageSettings -> FilmRoll
init imageSettings =
  empty{frsSettings =
          HashMap.insert imageSettings.filename imageSettings mempty
       }

insert :: ImageSettings -> FilmRoll -> FilmRoll
insert imageSettings filmRollSettings =
  filmRollSettings{frsSettings =
                     HashMap.insert
                       imageSettings.filename
                       imageSettings
                       filmRollSettings.frsSettings
                  }

fromList :: [ImageSettings] -> FilmRoll
fromList settings =
  empty{frsSettings =
          HashMap.fromList $
            fmap (\is -> (is.filename, is)) settings
       }

fromFilenames :: [Filename] -> FilmRoll
fromFilenames xs =
  empty{frsSettings =
          HashMap.fromList $
            fmap (\x -> (x, plainImageSettings x)) xs
       }

toList :: FilmRoll -> [ImageSettings]
toList =
  HashMap.elems . frsSettings

difference :: FilmRoll -> FilmRoll -> FilmRoll
difference (FilmRoll pa sa a) (FilmRoll pb sb b) =
  FilmRoll
    (pa <|> pb)
    (sa <> sb)
    (HashMap.differenceWith (\x y -> if x /= y then Just x else Nothing) a b)

plainImageSettings :: Filename -> ImageSettings
plainImageSettings x =
  ImageSettings x 0 noCrop 2.2 initZones 0 1 mempty

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
