{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Positive.Server.Api where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Media ((//))
import Positive.Data.Collection
import Positive.Data.FilmRoll
import Positive.Data.Id
import Positive.Data.ImageSettings
import Positive.Prelude hiding (ByteString)
import Servant
import Servant.API.Generic


-- API

data Api route = Api
  { settingsApi :: route :- ToServantApi SettingsApi
  , imageApi :: route :- ToServantApi ImageApi
  }
  deriving (Generic)


data ImageApi route = ImageApi
  { image ::
      route :- "image"
        :> QueryParam' '[Required, Strict] "dir" Text
        :> QueryParam' '[Required, Strict] "image-settings" ImageSettings
        :> Get '[Image] ByteString
  , events :: route :- "events" :> Raw
  , raw :: route :- Raw
  }
  deriving (Generic)


data SettingsApi route = SettingsApi
  -- FIXME: rename paths
  { saveFilmRoll ::
      route :- "film-roll"
        :> ReqBody '[JSON] FilmRoll
        :> Post '[JSON] FilmRoll
  , checkExpressions ::
      route :- "image" :> "settings" :> "expressions"
        :> ReqBody '[JSON] [Expression]
        :> Post '[JSON] [ExpressionResult]
  , getSettings ::
      route :- "image"
        :> "settings"
        :> Get '[JSON] [FilmRoll]
  , getCollections ::
      route :- "collection"
        :> Get '[JSON] [Collection]
  , addToCollection ::
      route :- "collection"
        :> Capture "collectionId" CollectionId
        :> Capture "imageSettingsId" ImageSettingsId
        :> Post '[JSON] [Collection]
  , removeFromCollection ::
      route :- "collection"
        :> Capture "collectionId" CollectionId
        :> Capture "imageSettingsId" ImageSettingsId
        :> Delete '[JSON] [Collection]
  , setCollectionTarget ::
      route :- "collection"
        :> Capture "collectionId" CollectionId
        :> Post '[JSON] [Collection]
  , exportCollection ::
      route :- "collection"
        :> Capture "collectionId" CollectionId
        :> "export"
        :> PostNoContent
  , getSettingsHistogram ::
      route :- "image" :> "settings" :> "histogram"
        :> ReqBody '[JSON] ImageSettings
        :> Post '[JSON] (Vector Int)
  , generateHighRes ::
      route :- "image" :> "settings" :> "highres"
        :> ReqBody '[JSON] ImageSettings
        :> PostNoContent
  , openExternalEditor ::
      route :- "image" :> "settings" :> "externaleditor"
        :> ReqBody '[JSON] ImageSettings
        :> PostNoContent
  , openInFinder ::
      route :- "image" :> "settings" :> "finder"
        :> Capture "imageSettingsId" ImageSettingsId
        :> PostNoContent
  , getCoordinateInfo ::
      route :- "image" :> "settings" :> "coordinate"
        :> ReqBody '[JSON] ([(Double, Double)], ImageSettings)
        :> Post '[JSON] [CoordinateInfo]
  , generateWallpaper ::
      route :- "image" :> "settings" :> "wallpaper"
        :> ReqBody '[JSON] ImageSettings
        :> PostNoContent
  }
  deriving (Generic)


-- IMAGE

data Image


instance Accept Image where
  contentType _ =
    "image" // "png"


instance MimeRender Image ByteString where
  mimeRender _ = identity
