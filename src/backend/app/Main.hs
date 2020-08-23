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
      flags@Flags {fIsDev, fDir} <- parseArgs
      when fIsDev (codeGen logger)
      log_ logger (tshow flags)
      let path = toFilePath fDir </> "image-settings.json"
      exists <- doesPathExist path
      if exists
        then server logger flags
        else do
          absolutePath <- (</> "image-settings.json") <$> makeAbsolute (toFilePath fDir)
          createSettingsFile <-
            Haskeline.runInputT Haskeline.defaultSettings . fmap (== Just 'y') . Haskeline.getInputChar $
              "Could not find " <> absolutePath <> ", press 'y' to create one now."
          when createSettingsFile $ do
            filenames <- fmap Text.pack . filter (Path.isExtensionOf ".png") <$> listDirectory (toFilePath fDir)
            let settings = fromFilenames filenames
            ByteString.writeFile path $ Aeson.encode settings
            log_ logger $ "Created: " <> tshow absolutePath
            server logger flags
