{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Positive.Static where

import Data.ByteString
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed
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
      case Wai.pathInfo req of
        [] -> resp $ Wai.responseLBS Http.status200 [] (fromStrict indexHtml)
        ["dist", "style.css"] -> resp $ Wai.responseLBS Http.status200 [] (fromStrict css)
        ["dist", "main.js"] -> resp $ Wai.responseLBS Http.status200 [] (fromStrict js)
        _ -> staticApp (defaultFileServerSettings "./") req resp

indexHtml :: ByteString
indexHtml =
  $(embedFile "./index.html")

css :: ByteString
css =
  $(embedFile "./dist/style.css")

js :: ByteString
js =
  $(embedFile "./dist/main.js")
