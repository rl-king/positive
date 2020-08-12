{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Foldable (for_)
import Data.HashMap.Strict as HashMap
import Data.List as List
import Data.Text as Text
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import qualified Language.Haskell.To.Elm as Elm
import Positive.Server
import Positive.Settings
import qualified Servant.To.Elm
import qualified System.Directory as Directory
import qualified System.Exit
import System.FilePath.Posix ((</>))
import qualified System.FilePath.Posix as Path
import qualified System.Process as Process

main :: IO ()
main = codeGen >> server

codeGen :: IO ()
codeGen = do
  let endpointDefinitions =
        fmap (Servant.To.Elm.elmEndpointDefinition (Expression.String "") ["Generated", "Request"]) $
          Servant.To.Elm.elmEndpoints @SettingsApi
      jsonDefinitions =
        Elm.jsonDefinitions @ImageSettings
      modules =
        Pretty.modules $
          Simplification.simplifyDefinition
            <$> jsonDefinitions <> endpointDefinitions
  putStrLn "Removing src/frontend/Generated before generating code"
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
    putStrLn $ "Wrote elm file: " <> (location </> filename)
  runElmFormat

runElmFormat :: IO ()
runElmFormat = do
  let args = ["--elm-version=0.19", "--yes", "src/frontend/Generated"]
  result <- Process.withCreateProcess (Process.proc "elm-format" args) $
    \_ _ _ handler -> Process.waitForProcess handler
  case result of
    System.Exit.ExitSuccess -> putStrLn "Formatted generated code with elm-format"
    _ -> putStrLn $ "Something went wrong trying to format the generated Elm code: " <> show result
