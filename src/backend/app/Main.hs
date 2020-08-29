{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import Positive.CodeGen
import Positive.Flags
import Positive.Prelude
import Positive.Server
import Positive.Settings
import qualified System.Console.Haskeline as Haskeline
import System.Directory
import System.FilePath.Posix as Path
import qualified System.Log.FastLogger as FastLogger

-- MAIN

main :: IO ()
main = do
  timeCache <- FastLogger.newTimeCache FastLogger.simpleTimeFormat
  FastLogger.withTimedFastLogger timeCache (FastLogger.LogStdout FastLogger.defaultBufSize) $
    \logger -> do
      flags@Flags {fIsDev, fDir, fInit} <- parseArgs
      let path = toFilePath fDir </> "image-settings.json"
      if fInit
        then do
          exists <- doesPathExist path
          if exists
            then log_ logger "Found image-settings.json, doing nothing."
            else do
              filenames <- fmap Text.pack . filter (Path.isExtensionOf ".png") <$> listDirectory (toFilePath fDir)
              let settings = fromFilenames filenames
              ByteString.writeFile path $ Aeson.encode settings
              createDirectoryIfMissing False (toFilePath fDir </> "previews")
              ByteString.writeFile (toFilePath fDir </> "previews" </> "image-settings.json") . Aeson.encode $ fromList []
              log_ logger "Wrote image-settings.json, and created previews dir"
        else do
          when fIsDev (codeGen logger)
          log_ logger (tshow flags)
          server logger flags
