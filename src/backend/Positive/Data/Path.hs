{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Positive.Data.Path (
  Path,
  Filename,
  Directory,
  unpack,
  pack,
  toFilePath,
  toByteString,
  fromFilePath,
  append,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Language.Haskell.To.Elm as Elm
import Positive.Prelude hiding (pack)
import Servant
import System.FilePath.Posix ((</>))


newtype Path (a :: Symbol)
  = Path Text
  deriving (Show, Eq, Generic, NFData, Hashable)
  deriving
    ( ToHttpApiData
    , FromHttpApiData
    , Aeson.ToJSON
    , Aeson.FromJSON
    )
    via Text


instance KnownSymbol a => Elm.HasElmType (Path a) where
  elmType =
    fromString $ "Data.Path." <> symbolVal (Proxy @a)


instance KnownSymbol a => Elm.HasElmDecoder Aeson.Value (Path a) where
  elmDecoder =
    "Data.Path.fromJson"


instance KnownSymbol a => Elm.HasElmEncoder Aeson.Value (Path a) where
  elmEncoder =
    "Data.Path.toJson"


instance KnownSymbol a => Elm.HasElmEncoder Text (Path a) where
  elmEncoder =
    "Data.Path.toString"


instance IsString (Path a) where
  fromString = fromFilePath


type Filename =
  Path "Filename"


type Directory =
  Path "Directory"


unpack :: Path a -> Text
unpack (Path id) = id


pack :: Text -> Path a
pack = Path


toByteString :: Path a -> ByteString
toByteString (Path x) = encodeUtf8 x


toFilePath :: Path a -> FilePath
toFilePath (Path x) = Text.unpack x


fromFilePath :: FilePath -> Path a
fromFilePath = Path . Text.pack


append :: Path a -> Path b -> FilePath
append dir filename =
  toFilePath dir </> toFilePath filename
