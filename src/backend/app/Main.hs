{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Positive.CodeGen as CodeGen
import Positive.Flags (Flags (..))
import qualified Positive.Flags as Flags
import Positive.Prelude
import qualified Positive.Preview as Preview
import Positive.Server
import Positive.Settings
import System.Directory
import System.FilePath.Posix as Path
import qualified System.Log.FastLogger as FastLogger

-- MAIN

main :: IO ()
main = do
  timeCache <- FastLogger.newTimeCache FastLogger.simpleTimeFormat
  FastLogger.withTimedFastLogger timeCache (FastLogger.LogStdout FastLogger.defaultBufSize) $
    \logger -> do
      flags@Flags {fIsDev, fMode} <- Flags.parseArgs
      let path = "image-settings.json"
      case fMode of
        Flags.Init -> do
          exists <- doesPathExist path
          if exists
            then log_ logger "Found image-settings.json, doing nothing."
            else do
              filenames <- fmap Text.pack . filter (Path.isExtensionOf ".png") <$> listDirectory "."
              let settings = fromFilenames filenames
              ByteString.writeFile path $ Aeson.encode settings
              createDirectoryIfMissing False "previews"
              ByteString.writeFile ("previews" </> "image-settings.json") . Aeson.encode $ fromList []
              log_ logger "Wrote image-settings.json, and created previews dir"
        Flags.Previews ->
          Preview.run
        Flags.ContactSheet ->
          Preview.run
        Flags.Server -> do
          when fIsDev (CodeGen.run logger)
          log_ logger (tshow flags)
          server logger flags
