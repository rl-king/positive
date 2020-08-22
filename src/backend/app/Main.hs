{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Positive.CodeGen
import Positive.Effect.Log
import Positive.Flags
import Positive.Prelude
import Positive.Server
import qualified System.Log.FastLogger as FastLogger

-- MAIN

main :: IO ()
main = do
  timeCache <- FastLogger.newTimeCache FastLogger.simpleTimeFormat
  FastLogger.withTimedFastLogger timeCache (FastLogger.LogStdout FastLogger.defaultBufSize) $
    \logger -> do
      flags@Flags {fIsDev} <- parseArgs
      when fIsDev (codeGen logger)
      log_ logger (tshow flags)
      server logger flags
