{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Positive.Api where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Media ((//))
import Positive.Data.FilmRoll
import Positive.Data.ImageSettings
import Positive.Prelude hiding (ByteString)
import Servant
import Servant.API.Generic

-- API

data Api route = Api
  { settingsApi :: route :- ToServantApi SettingsApi,
    imageApi :: route :- ToServantApi ImageApi
  }
  deriving (Generic)

data ImageApi route = ImageApi
  { image ::
      route :- "image"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> QueryParam' '[Required, Strict] "image-settings" ImageSettings
        :> Get '[Image] ByteString,
    events :: route :- "events" :> Raw,
    raw :: route :- Raw
  }
  deriving (Generic)

data SettingsApi route = SettingsApi
  { saveFilmRoll ::
      route :- "film-roll"
        :> Capture "filmRollId" Int32
        :> ReqBody '[JSON] FilmRoll
        :> Post '[JSON] FilmRoll,
    checkExpressions ::
      route :- "image" :> "settings" :> "expressions"
        :> ReqBody '[JSON] [Expression]
        :> Post '[JSON] [ExpressionResult],
    getSettings ::
      route :- "image" :> "settings"
        :> Get '[JSON] [(Text, FilmRoll)],
    getSettingsHistogram ::
      route :- "image" :> "settings" :> "histogram"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] ImageSettings
        :> Post '[JSON] [Int],
    generateHighRes ::
      route :- "image" :> "settings" :> "highres"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] ImageSettings
        :> PostNoContent '[JSON] NoContent,
    openExternalEditor ::
      route :- "image" :> "settings" :> "externaleditor"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] ImageSettings
        :> PostNoContent '[JSON] NoContent,
    getCoordinateInfo ::
      route :- "image" :> "settings" :> "coordinate"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> ReqBody '[JSON] ([(Double, Double)], ImageSettings)
        :> Post '[JSON] [CoordinateInfo],
    generateWallpaper ::
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
  mimeRender _ = identity
