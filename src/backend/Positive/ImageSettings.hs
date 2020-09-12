{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Positive.ImageSettings where

import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Bifunctor
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import qualified Data.Text as Text
import qualified Generics.SOP as SOP
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Type as Type
import qualified Language.Haskell.To.Elm as Elm
import qualified Network.HTTP.Media as Media
import Positive.Prelude hiding (ByteString)
import Servant
import qualified System.FilePath.Glob as Glob
import System.FilePath.Posix as Path

-- IMAGE

data Image

instance Accept Image where
  contentType _ =
    "image" Media.// "jpg"

instance MimeRender Image ByteString where
  mimeRender _ = id

-- FILMROLLSETTINGS

data FilmRollSettings = FilmRollSettings
  { frsPoster :: Maybe Text,
    frsStars :: HashMap Text Int,
    frsSettings :: HashMap Text ImageSettings
  }
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Show, Eq)

instance Aeson.FromJSON FilmRollSettings where
  parseJSON =
    Aeson.withObject "FilmRollSettings" $ \o -> do
      poster <- o .:? "frsPoster"
      stars <- o .:? "frsStars" .!= mempty
      settings <- o .: "unFilmRollSettings" <|> o .: "frsSettings"
      pure $
        FilmRollSettings
          { frsPoster = poster,
            frsStars = stars,
            frsSettings = settings
          }

instance Aeson.ToJSON FilmRollSettings where
  toJSON FilmRollSettings {frsPoster, frsStars, frsSettings} =
    Aeson.object
      [ "frsPoster" .= frsPoster,
        "frsSettings" .= frsSettings,
        "frsStars" .= frsStars
      ]

empty :: FilmRollSettings
empty =
  FilmRollSettings Nothing mempty mempty

isEmpty :: FilmRollSettings -> Bool
isEmpty =
  (==) empty

init :: ImageSettings -> FilmRollSettings
init imageSettings =
  FilmRollSettings Nothing mempty (HashMap.insert (iFilename imageSettings) imageSettings mempty)

insert :: ImageSettings -> FilmRollSettings -> FilmRollSettings
insert imageSettings FilmRollSettings {frsPoster, frsStars, frsSettings} =
  FilmRollSettings frsPoster frsStars (HashMap.insert (iFilename imageSettings) imageSettings frsSettings)

fromList :: [ImageSettings] -> FilmRollSettings
fromList settings =
  FilmRollSettings Nothing mempty (HashMap.fromList $ fmap (\is -> (iFilename is, is)) settings)

fromFilenames :: [Text] -> FilmRollSettings
fromFilenames xs =
  FilmRollSettings
    Nothing
    mempty
    (HashMap.fromList $ fmap (\x -> (x, ImageSettings x 0 noCrop 2.2 0 0 0 0 1)) xs)

toList :: FilmRollSettings -> [ImageSettings]
toList =
  HashMap.elems . frsSettings

difference :: FilmRollSettings -> FilmRollSettings -> FilmRollSettings
difference (FilmRollSettings pa sa a) (FilmRollSettings pb sb b) =
  FilmRollSettings
    (pa <|> pb)
    (sa <> sb)
    (HashMap.differenceWith (\x y -> if x /= y then Just x else Nothing) a b)

instance Elm.HasElmType FilmRollSettings where
  elmDefinition =
    Just $ Elm.deriveElmTypeDefinition @FilmRollSettings Elm.defaultOptions "Generated.Data.ImageSettings.FilmRollSettings"

instance Elm.HasElmDecoder Aeson.Value FilmRollSettings where
  elmDecoderDefinition =
    Just $ Elm.deriveElmJSONDecoder @FilmRollSettings Elm.defaultOptions Aeson.defaultOptions "Generated.Data.ImageSettings.decodeFilmRollSettings"

instance Elm.HasElmEncoder Aeson.Value FilmRollSettings where
  elmEncoderDefinition =
    Just $ Elm.deriveElmJSONEncoder @FilmRollSettings Elm.defaultOptions Aeson.defaultOptions "Generated.Data.ImageSettings.encodeFilmRollSettings"

instance
  (ElmComparable k, Elm.HasElmType k, Elm.HasElmType a) =>
  Elm.HasElmType (HashMap k a)
  where
  elmType =
    Type.apps "Dict.Dict" [Elm.elmType @k, Elm.elmType @a]

instance
  (ElmComparable k, Elm.HasElmEncoder Text k, Elm.HasElmEncoder Aeson.Value a) =>
  Elm.HasElmEncoder Aeson.Value (HashMap k a)
  where
  elmEncoder =
    Expression.apps "Json.Encode.dict" [Elm.elmEncoder @Text @k, Elm.elmEncoder @Aeson.Value @a]

instance
  (ElmComparable k, Elm.HasElmDecoder Aeson.Value k, Elm.HasElmDecoder Aeson.Value a) =>
  Elm.HasElmDecoder Aeson.Value (HashMap k a)
  where
  elmDecoder =
    Expression.App "Json.Decode.dict" (Elm.elmDecoder @Aeson.Value @a)

-- SET

instance (Elm.HasElmType a, ElmComparable a) => Elm.HasElmType (HashSet a) where
  elmType =
    Type.App "Set.Set" (Elm.elmType @a)

instance (Elm.HasElmEncoder Aeson.Value a, ElmComparable a) => Elm.HasElmEncoder Aeson.Value (HashSet a) where
  elmEncoder =
    Expression.App "Json.Encode.set" (Elm.elmEncoder @Aeson.Value @a)

instance (Elm.HasElmDecoder Aeson.Value a, ElmComparable a) => Elm.HasElmDecoder Aeson.Value (HashSet a) where
  elmDecoder =
    Expression.apps
      "Json.Decode.map"
      [ "Set.fromList",
        Expression.App "Json.Decode.list" (Elm.elmDecoder @Aeson.Value @a)
      ]

class ElmComparable a

instance ElmComparable Text

-- IMAGESETTINGS

data ImageSettings = ImageSettings
  { iFilename :: Text,
    iRotate :: Double,
    iCrop :: ImageCrop,
    iGamma :: Double,
    iZone1 :: Double,
    iZone5 :: Double,
    iZone9 :: Double,
    iBlackpoint :: Double,
    iWhitepoint :: Double
  }
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance FromHttpApiData ImageSettings where
  parseUrlPiece piece =
    first Text.pack $
      Aeson.eitherDecodeStrict =<< Base64.decode (encodeUtf8 piece)
  parseQueryParam = parseUrlPiece

instance Elm.HasElmType ImageSettings where
  elmDefinition =
    Just $ Elm.deriveElmTypeDefinition @ImageSettings Elm.defaultOptions "Generated.Data.ImageSettings.ImageSettings"

instance Elm.HasElmDecoder Aeson.Value ImageSettings where
  elmDecoderDefinition =
    Just $ Elm.deriveElmJSONDecoder @ImageSettings Elm.defaultOptions Aeson.defaultOptions "Generated.Data.ImageSettings.decodeImageSettings"

instance Elm.HasElmEncoder Aeson.Value ImageSettings where
  elmEncoderDefinition =
    Just $ Elm.deriveElmJSONEncoder @ImageSettings Elm.defaultOptions Aeson.defaultOptions "Generated.Data.ImageSettings.encodeImageSettings"

-- CROP

data ImageCrop = ImageCrop
  { icTop :: Double,
    icLeft :: Double,
    icWidth :: Double
  }
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

noCrop :: ImageCrop
noCrop =
  ImageCrop 0 0 100

instance Elm.HasElmType ImageCrop where
  elmDefinition =
    Just $ Elm.deriveElmTypeDefinition @ImageCrop Elm.defaultOptions "Generated.Data.ImageSettings.ImageCrop"

instance Elm.HasElmDecoder Aeson.Value ImageCrop where
  elmDecoderDefinition =
    Just $ Elm.deriveElmJSONDecoder @ImageCrop Elm.defaultOptions Aeson.defaultOptions "Generated.Data.ImageSettings.decodeImageCrop"

instance Elm.HasElmEncoder Aeson.Value ImageCrop where
  elmEncoderDefinition =
    Just $ Elm.deriveElmJSONEncoder @ImageCrop Elm.defaultOptions Aeson.defaultOptions "Generated.Data.ImageSettings.encodeImageCrop"

-- FS

findImageSettings :: IO (HashMap Text FilmRollSettings)
findImageSettings = do
  settingFiles <- findImageSettingFiles
  filmRollSettings <-
    traverse
      (\path -> (,) <$> pure (Text.pack (makeRelative "./" (takeDirectory path))) <*> Aeson.decodeFileStrict path)
      settingFiles
  pure $
    foldr
      (\(path, maybeSettings) acc -> maybe acc (\settings -> HashMap.insert path settings acc) maybeSettings)
      mempty
      filmRollSettings

findImageSettingFiles :: IO [FilePath]
findImageSettingFiles =
  (++)
    <$> Glob.glob "./**/[Roll]*/image-settings.json"
    <*> Glob.glob "./image-settings.json"

diffedPreviewSettings :: FilePath -> FilePath -> IO FilmRollSettings
diffedPreviewSettings a b = do
  xs <- Aeson.decodeFileStrict $ a </> "image-settings.json"
  ys <- Aeson.decodeFileStrict $ b </> "image-settings.json"
  maybe (fail "Something went wrong decoding the settings files") pure $
    difference <$> xs <*> ys

insertPreviewSettings :: FilePath -> ImageSettings -> IO ()
insertPreviewSettings ps settings = do
  previewSettings <- Aeson.decodeFileStrict ps
  maybe
    (fail "Something went wrong decoding the settings file")
    (Aeson.encodeFile ps . insert settings)
    previewSettings
