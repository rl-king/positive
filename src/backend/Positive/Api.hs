{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Positive.Api where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Media ((//))
import Positive.FilmRoll
import Positive.Image.Settings
import Positive.Prelude hiding (ByteString)
import Servant
import Servant.API.Generic

-- API

data Api route = Api
  { aSettingsApi :: route :- ToServantApi SettingsApi,
    aImageApi :: route :- ToServantApi ImageApi
  }
  deriving (Generic)

data ImageApi route = ImageApi
  { iaImage ::
      route :- "image"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> QueryParam' '[Required, Strict] "image-settings" Settings
        :> Get '[Image] ByteString,
    iaEvents :: route :- "events" :> Raw,
    iaRaw :: route :- Raw
  }
  deriving (Generic)

data SettingsApi route = SettingsApi
  { saSaveSettings ::
      route :- "image" :> "settings"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] FilmRoll
        :> Post '[JSON] FilmRoll,
    saCheckExpressions ::
      route :- "image" :> "settings" :> "expressions"
        :> ReqBody '[JSON] [Expression]
        :> Post '[JSON] [ExpressionResult],
    saGetSettings ::
      route :- "image" :> "settings"
        :> Get '[JSON] [(Text, FilmRoll)],
    saGetSettingsHistogram ::
      route :- "image" :> "settings" :> "histogram"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] Settings
        :> Post '[JSON] [Int],
    saGenerateHighRes ::
      route :- "image" :> "settings" :> "highres"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] Settings
        :> PostNoContent '[JSON] NoContent,
    saOpenExternalEditor ::
      route :- "image" :> "settings" :> "externaleditor"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] Settings
        :> PostNoContent '[JSON] NoContent,
    saGetCoordinateInfo ::
      route :- "image" :> "settings" :> "coordinate"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] ([(Double, Double)], Settings)
        :> Post '[JSON] [CoordinateInfo],
    saGenerateWallpaper ::
      route :- "image" :> "settings" :> "wallpaper"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] Settings
        :> PostNoContent '[JSON] NoContent
  }
  deriving (Generic)

-- IMAGE

data Image

instance Accept Image where
  contentType _ =
    "image" // "png"

instance MimeRender Image ByteString where
  mimeRender _ = id
