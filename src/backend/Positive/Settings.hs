{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Positive.Settings where

import qualified Data.Aeson as Aeson
import Data.Bifunctor
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import qualified Data.Text as Text
import qualified Generics.SOP as SOP
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

newtype FilmRollSettings = FilmRollSettings
  { unFilmRollSettings :: HashMap Text ImageSettings
  }
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

empty :: FilmRollSettings
empty =
  FilmRollSettings mempty

isEmpty :: FilmRollSettings -> Bool
isEmpty =
  (==) empty

init :: ImageSettings -> FilmRollSettings
init imageSettings =
  FilmRollSettings $ HashMap.insert (iFilename imageSettings) imageSettings mempty

insert :: ImageSettings -> FilmRollSettings -> FilmRollSettings
insert imageSettings =
  FilmRollSettings . HashMap.insert (iFilename imageSettings) imageSettings . unFilmRollSettings

fromList :: [ImageSettings] -> FilmRollSettings
fromList =
  FilmRollSettings . HashMap.fromList . fmap (\is -> (iFilename is, is))

fromFilenames :: [Text] -> FilmRollSettings
fromFilenames =
  FilmRollSettings
    . HashMap.fromList
    . fmap (\x -> (x, ImageSettings x 0 noCrop 2.2 0 0 0 0 1))

toList :: FilmRollSettings -> [ImageSettings]
toList =
  HashMap.elems . unFilmRollSettings

grab :: FilmRollSettings -> Maybe (ImageSettings, FilmRollSettings)
grab filmRoll =
  case sortOn (Down . iFilename) (toList filmRoll) of
    [] -> Nothing
    x : xs -> Just (x, fromList xs)

difference :: FilmRollSettings -> FilmRollSettings -> FilmRollSettings
difference (FilmRollSettings a) (FilmRollSettings b) =
  FilmRollSettings $
    HashMap.differenceWith (\x y -> if x /= y then Just x else Nothing) a b

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

-- DIR

newtype FilmRollDir = FilmRollDir {unFilmRollDirectory :: Text}
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

toFilePath :: FilmRollDir -> FilePath
toFilePath =
  Text.unpack . unFilmRollDirectory

instance Elm.HasElmType FilmRollDir where
  elmDefinition =
    Just $ Elm.deriveElmTypeDefinition @FilmRollDir Elm.defaultOptions "Generated.Data.ImageSettings.FilmRollDir"

instance Elm.HasElmDecoder Aeson.Value FilmRollDir where
  elmDecoderDefinition =
    Just $ Elm.deriveElmJSONDecoder @FilmRollDir Elm.defaultOptions Aeson.defaultOptions "Generated.Data.ImageSettings.decodeFilmRollDirectory"

instance Elm.HasElmEncoder Aeson.Value FilmRollDir where
  elmEncoderDefinition =
    Just $ Elm.deriveElmJSONEncoder @FilmRollDir Elm.defaultOptions Aeson.defaultOptions "Generated.Data.ImageSettings.encodeFilmRollDirectory"

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
  Glob.glob "./**/[Roll]*/image-settings.json"

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
