{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Positive.Settings where

import qualified Data.Aeson as Aeson
import Data.Bifunctor
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Generics.SOP as SOP
import qualified Language.Haskell.To.Elm as Elm
import qualified Network.HTTP.Media as Media
import Positive.Prelude hiding (ByteString)
import Servant

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

newtype WorkingDirectory = WorkingDirectory {unWorkingDirectory :: Text}
  deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

toFilePath :: WorkingDirectory -> FilePath
toFilePath =
  Text.unpack . unWorkingDirectory

instance Elm.HasElmType WorkingDirectory where
  elmDefinition =
    Just $ Elm.deriveElmTypeDefinition @WorkingDirectory Elm.defaultOptions "Generated.Data.ImageSettings.WorkingDirectory"

instance Elm.HasElmDecoder Aeson.Value WorkingDirectory where
  elmDecoderDefinition =
    Just $ Elm.deriveElmJSONDecoder @WorkingDirectory Elm.defaultOptions Aeson.defaultOptions "Generated.Data.ImageSettings.decodeWorkingDirectory"

instance Elm.HasElmEncoder Aeson.Value WorkingDirectory where
  elmEncoderDefinition =
    Just $ Elm.deriveElmJSONEncoder @WorkingDirectory Elm.defaultOptions Aeson.defaultOptions "Generated.Data.ImageSettings.encodeWorkingDirectory"
