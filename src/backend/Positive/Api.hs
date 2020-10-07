{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Positive.Api where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Media ((//))
import Positive.ImageSettings
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
        :> QueryParam' '[Required, Strict] "image-settings" ImageSettings
        :> Get '[Image] ByteString,
    iaEvents :: route :- "events" :> Raw,
    iaRaw :: route :- Raw
  }
  deriving (Generic)

data SettingsApi route = SettingsApi
  { saSaveSettings ::
      route :- "image" :> "settings"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] FilmRollSettings
        :> Post '[JSON] FilmRollSettings,
    saGetSettings ::
      route :- "image" :> "settings"
        :> Get '[JSON] [(Text, FilmRollSettings)],
    saGetSettingsHistogram ::
      route :- "image" :> "settings" :> "histogram"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] ImageSettings
        :> Post '[JSON] [Int],
    saGenerateHighRes ::
      route :- "image" :> "settings" :> "highres"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] ImageSettings
        :> PostNoContent '[JSON] NoContent,
    saGetCoordinateInfo ::
      route :- "image" :> "settings" :> "coordinate"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] ([(Double, Double)], ImageSettings)
        :> Post '[JSON] [CoordinateInfo],
    saGenerateWallpaper ::
      route :- "image" :> "settings" :> "wallpaper"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] ImageSettings
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
