{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Positive.CodeGen as CodeGen
import qualified Positive.Contacts as Contacts
import Positive.Flags (Flags (..))
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
      flags@Flags {fIsDev, fMode, fHard} <- Flags.parseArgs
      let log = Log.log logger
      case fMode of
        Flags.Init ->
          Init.run fHard >> Preview.run log -- >> Contacts.run log
        Flags.Previews ->
          Preview.run log
        Flags.Contacts ->
          Contacts.run log
        Flags.SingleImage filepath ->
          SingleImage.run log filepath
        Flags.Server ->
          when fIsDev (CodeGen.run log) >> Server.run logger flags
