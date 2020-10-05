{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Main where

import qualified Positive.CodeGen as CodeGen
import qualified Positive.Contacts as Contacts
import qualified Positive.Flags as Flags
import qualified Positive.Init as Init
import qualified Positive.Log as Log
import Positive.Prelude
import qualified Positive.Preview as Preview
import qualified Positive.Server as Server
import qualified Positive.SingleImage as SingleImage
import qualified System.Log.FastLogger as FastLogger

-- MAIN

main :: IO ()
main = do
  timeCache <- FastLogger.newTimeCache FastLogger.simpleTimeFormat
  FastLogger.withTimedFastLogger timeCache (FastLogger.LogStdout FastLogger.defaultBufSize) $
    \logger -> do
      flags <- Flags.parseArgs
      let log = Log.log logger
      case flags.mode of
        Flags.Init ->
          Init.run flags.hard >> Preview.run log -- >> Contacts.run log
        Flags.Previews ->
          Preview.run log
        Flags.Contacts ->
          Contacts.run log
        Flags.SingleImage filepath ->
          SingleImage.run log filepath
        Flags.Server ->
          when flags.isDev (CodeGen.run log) >> Server.run logger flags
