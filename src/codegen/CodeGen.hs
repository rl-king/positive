{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import qualified Language.Haskell.To.Elm as Elm
import Positive.Data.FilmRoll
import Positive.Data.ImageSettings
import Positive.Prelude
import Positive.Server.Api
import Servant.API.Generic (ToServantApi)
import qualified Servant.To.Elm
import qualified System.Directory as Directory
import qualified System.Exit
import System.FilePath.Posix ((</>))
import qualified System.FilePath.Posix as Path
import qualified System.Process as Process

-- CODEGEN

main :: IO ()
main = do
  let httpDefs =
        fmap (Servant.To.Elm.elmEndpointDefinition (Expression.String "") ["Generated", "Request"]) $
          Servant.To.Elm.elmEndpoints @(ToServantApi SettingsApi)
      typeDefs =
        Elm.jsonDefinitions @ImageSettings
          <> Elm.jsonDefinitions @ImageCrop
          <> Elm.jsonDefinitions @FilmRoll
          <> Elm.jsonDefinitions @Zones
          <> Elm.jsonDefinitions @CoordinateInfo
          <> Elm.jsonDefinitions @Expression
          <> Elm.jsonDefinitions @ExpressionResult
      modules =
        -- FIXME: spaceleak in both elm-syntax functions
        Pretty.modules $
          Simplification.simplifyDefinition <$> typeDefs <> httpDefs
  whenM (Directory.doesDirectoryExist "src/frontend/Generated") $ do
    Text.putStrLn "Removing src/frontend/Generated before generating code"
    Directory.removeDirectoryRecursive "src/frontend/Generated"
  for_ (HashMap.toList modules) $ \(modulePath, contents) -> do
    (filename, location) <-
      case List.reverse modulePath of
        [] -> pure ("", "")
        [moduleName] -> do
          let p = "src/frontend"
          Directory.createDirectoryIfMissing True p
          pure (Text.unpack $ moduleName <> ".elm", p)
        (moduleName : rest) -> do
          let p = Path.joinPath . fmap Text.unpack $ "src" : "frontend" : List.reverse rest
          Directory.createDirectoryIfMissing True p
          pure (Text.unpack $ moduleName <> ".elm", p)
    writeFile (location </> filename) (show contents)
    Text.putStrLn $ "Wrote elm file: " <> Text.pack (location </> filename)
  runElmFormat

runElmFormat :: IO ()
runElmFormat =
  let args = ["--elm-version=0.19", "--yes", "src/frontend/Generated"]
   in Process.withCreateProcess (Process.proc "elm-format" args) $
        \_ _ _ handler ->
          Process.waitForProcess handler >>= \case
            System.Exit.ExitSuccess ->
              Text.putStrLn "Formatted generated code with elm-format"
            result ->
              Text.putStrLn $
                "Something went wrong trying to format the generated Elm code: "
                  <> tshow result
