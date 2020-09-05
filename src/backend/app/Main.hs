{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Positive.CodeGen as CodeGen
import qualified Positive.ContactSheet as ContactSheet
import Positive.Flags (Flags (..))
import qualified Positive.Flags as Flags
import qualified Positive.Init as Init
import Positive.Prelude
import qualified Positive.Preview as Preview
import Positive.Server
import qualified System.Log.FastLogger as FastLogger

-- MAIN

main :: IO ()
main = do
  timeCache <- FastLogger.newTimeCache FastLogger.simpleTimeFormat
  FastLogger.withTimedFastLogger timeCache (FastLogger.LogStdout FastLogger.defaultBufSize) $
    \logger -> do
      flags@Flags {fIsDev, fMode} <- Flags.parseArgs
      case fMode of
        Flags.Init ->
          Init.run False >> Preview.run >> ContactSheet.run
        Flags.Reset ->
          Init.run True >> Preview.run >> ContactSheet.run
        Flags.Previews ->
          Preview.run
        Flags.ContactSheet ->
          ContactSheet.run
        Flags.Server ->
          when fIsDev (CodeGen.run logger) >> server logger flags
