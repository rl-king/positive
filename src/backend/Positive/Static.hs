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
serve isDev
  | isDev =
    pure $ \req resp -> staticApp (defaultFileServerSettings "./") req resp
  | otherwise =
    pure $ \req resp ->
      let assets = HashMap.fromList (fmap (first Text.pack) static)
       in case Wai.pathInfo req of
            [] -> resp $ Wai.responseLBS Http.status200 [] (fromStrict indexHtml)
            "dist" : rest ->
              resp
                . maybe
                  (Wai.responseLBS Http.status404 [] "Not found - 404")
                  (Wai.responseLBS Http.status200 [] . fromStrict)
                $ HashMap.lookup (Text.intercalate "/" rest) assets
            _ -> staticApp (defaultFileServerSettings "./") req resp

indexHtml :: ByteString
indexHtml =
  $(embedFile "./index.html")

static :: [(FilePath, ByteString)]
static =
  $(embedDir "./dist")
