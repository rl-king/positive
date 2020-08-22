{-# LANGUAGE TypeApplications #-}

module Positive.CodeGen where

import Data.HashMap.Strict as HashMap
import Data.List as List
import Data.Text as Text
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import qualified Language.Haskell.To.Elm as Elm
import Positive.Effect.Log
import Positive.Prelude
import Positive.Server
import Positive.Settings
import Servant.API.Generic (ToServantApi)
import qualified Servant.To.Elm
import qualified System.Directory as Directory
import qualified System.Exit
import System.FilePath.Posix ((</>))
import qualified System.FilePath.Posix as Path
import System.Log.FastLogger (TimedFastLogger)
import qualified System.Process as Process

-- CODEGEN

codeGen :: TimedFastLogger -> IO ()
codeGen logger = do
  let endpointDefinitions =
        fmap (Servant.To.Elm.elmEndpointDefinition (Expression.String "") ["Generated", "Request"]) $
          Servant.To.Elm.elmEndpoints @(ToServantApi SettingsApi)
      jsonDefinitions =
        Elm.jsonDefinitions @ImageSettings
          <> Elm.jsonDefinitions @ImageCrop
          <> Elm.jsonDefinitions @PathSegment
      modules =
        Pretty.modules $
          Simplification.simplifyDefinition
            <$> jsonDefinitions <> endpointDefinitions
  log_ logger "Removing src/frontend/Generated before generating code"
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
    log_ logger $ "Wrote elm file: " <> Text.pack (location </> filename)
  runElmFormat logger

runElmFormat :: TimedFastLogger -> IO ()
runElmFormat logger = do
  let args = ["--elm-version=0.19", "--yes", "src/frontend/Generated"]
  result <- Process.withCreateProcess (Process.proc "elm-format" args) $
    \_ _ _ handler -> Process.waitForProcess handler
  case result of
    System.Exit.ExitSuccess -> log_ logger "Formatted generated code with elm-format"
    _ -> log_ logger $ "Something went wrong trying to format the generated Elm code: " <> tshow result
