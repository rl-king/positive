{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Positive.ImageSettings where

import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Bifunctor
import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import qualified Data.Text as Text
import qualified Generics.SOP as SOP
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Type as Type
import qualified Language.Haskell.To.Elm as Elm
import Positive.Prelude
import Servant
import qualified System.FilePath.Glob as Glob
import System.FilePath.Posix as Path

-- FILMROLLSETTINGS

data FilmRollSettings = FilmRollSettings
  { frsPoster :: !(Maybe Text),
    frsRatings :: !(HashMap Text Int),
    frsSettings :: !(HashMap Text ImageSettings)
  }
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Show, Eq)

instance Aeson.FromJSON FilmRollSettings where
  parseJSON =
    Aeson.withObject "FilmRollSettings" $ \o -> do
      poster <- o .:? "frsPoster"
      ratings <- o .:? "frsRatings" .!= mempty
      settings <- o .: "unFilmRollSettings" <|> o .: "frsSettings"
      pure $
        FilmRollSettings
          { frsPoster = poster,
            frsRatings = ratings,
            frsSettings = settings
          }

instance Aeson.ToJSON FilmRollSettings where
  toJSON filmRollSettings =
    Aeson.object
      [ "frsPoster" .= filmRollSettings.frsPoster,
        "frsSettings" .= filmRollSettings.frsSettings,
        "frsRatings" .= filmRollSettings.frsRatings
      ]

empty :: FilmRollSettings
empty =
  FilmRollSettings Nothing mempty mempty

isEmpty :: FilmRollSettings -> Bool
isEmpty =
  (==) empty

init :: ImageSettings -> FilmRollSettings
init imageSettings =
  empty{frsSettings = HashMap.insert imageSettings.iFilename imageSettings mempty}

insert :: ImageSettings -> FilmRollSettings -> FilmRollSettings
insert imageSettings filmRollSettings =
  filmRollSettings{frsSettings = HashMap.insert imageSettings.iFilename imageSettings filmRollSettings.frsSettings}

fromList :: [ImageSettings] -> FilmRollSettings
fromList settings =
  empty{frsSettings = HashMap.fromList $ fmap (\is -> (iFilename is, is)) settings}

fromFilenames :: [Text] -> FilmRollSettings
fromFilenames xs =
  empty{frsSettings = HashMap.fromList $ fmap (\x -> (x, plainImageSettings x)) xs}

toList :: FilmRollSettings -> [ImageSettings]
toList =
  HashMap.elems . frsSettings

difference :: FilmRollSettings -> FilmRollSettings -> FilmRollSettings
difference (FilmRollSettings pa sa a) (FilmRollSettings pb sb b) =
  FilmRollSettings
    (pa <|> pb)
    (sa <> sb)
    (HashMap.differenceWith (\x y -> if x /= y then Just x else Nothing) a b)

plainImageSettings :: Text -> ImageSettings
plainImageSettings x =
  ImageSettings x 0 noCrop 2.2 initZones 0 1

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

class ElmComparable a

instance ElmComparable Text

-- IMAGESETTINGS

data ImageSettings = ImageSettings
  { iFilename :: !Text,
    iRotate :: !Double,
    iCrop :: !ImageCrop,
    iGamma :: !Double,
    iZones :: !Zones,
    iBlackpoint :: !Double,
    iWhitepoint :: !Double
  }
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Show, Eq, Aeson.ToJSON)

instance Aeson.FromJSON ImageSettings where
  parseJSON =
    Aeson.withObject "ImageSettings" $ \o -> do
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
      pure $
        ImageSettings
          { iFilename = filename,
            iRotate = rotate,
            iCrop = crop,
            iGamma = gamma,
            iZones = zones,
            iBlackpoint = blackpoint,
            iWhitepoint = whitepoint
          }

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
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

initZones :: Zones
initZones =
  Zones 0 0 0 0 0 0 0 0 0

instance Elm.HasElmType Zones where
  elmDefinition =
    Just $ Elm.deriveElmTypeDefinition @Zones Elm.defaultOptions "Generated.Data.ImageSettings.Zones"

instance Elm.HasElmDecoder Aeson.Value Zones where
  elmDecoderDefinition =
    Just $ Elm.deriveElmJSONDecoder @Zones Elm.defaultOptions Aeson.defaultOptions "Generated.Data.ImageSettings.decodeZones"

instance Elm.HasElmEncoder Aeson.Value Zones where
  elmEncoderDefinition =
    Just $ Elm.deriveElmJSONEncoder @Zones Elm.defaultOptions Aeson.defaultOptions "Generated.Data.ImageSettings.encodeZones"

-- CROP

data ImageCrop = ImageCrop
  { icTop :: !Double,
    icLeft :: !Double,
    icWidth :: !Double
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

pickFilename :: FilePath -> IO FilePath
pickFilename filepath = do
  current <- Glob.glob (dropExtension filepath <> "*")
  pure $
    if null current
      then filepath
      else
        let toNumbers = read @Int . reverse . takeWhile isDigit . reverse . dropExtension
         in case sortOn Down $ toNumbers <$> filter (/= filepath) current of
              n : _ ->
                mconcat [dropExtension filepath, "-", show (n + 1), takeExtension filepath]
              _ ->
                mconcat [dropExtension filepath, "-1", takeExtension filepath]
