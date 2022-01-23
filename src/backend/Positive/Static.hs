{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Positive.Static where

import Data.ByteString
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
import Positive.Prelude
import Servant


serve :: Bool -> Tagged (m :: Type -> Type) Application
serve True =
  pure $ \req resp ->
    case Wai.pathInfo req of
      [] ->
        resp $ Wai.responseFile Http.status200 [] "./index.html" Nothing
      "editor" : _ ->
        resp $ Wai.responseFile Http.status200 [] "./index.html" Nothing
      "dist" : _ ->
        staticApp (defaultFileServerSettings "./") req resp
      _ ->
        staticApp (defaultFileServerSettings "/") req resp
serve False =
  pure $ \req resp -> do
    let assets = HashMap.fromList (fmap (first Text.pack) static)
     in case Wai.pathInfo req of
          "dist" : "icons" : rest ->
            resp $ toFile [(Http.hContentType, "image/svg+xml")] ("icons" : rest) assets
          "dist" : rest ->
            resp $ toFile [] rest assets
          "editor" : _ ->
            resp $ Wai.responseLBS Http.status200 [] (fromStrict indexHtml)
          [] ->
            resp $ Wai.responseLBS Http.status200 [] (fromStrict indexHtml)
          _ ->
            staticApp (defaultFileServerSettings "/") req resp


toFile :: Http.ResponseHeaders -> [Text] -> HashMap Text ByteString -> Wai.Response
toFile headers path assets =
  maybe
    (Wai.responseLBS Http.status404 [] "Not found - 404")
    (Wai.responseLBS Http.status200 headers . fromStrict)
    $ HashMap.lookup (Text.intercalate "/" path) assets


indexHtml :: ByteString
indexHtml =
  $(embedFile "dist/index.html")


static :: [(FilePath, ByteString)]
static =
  $(embedDir "dist")
