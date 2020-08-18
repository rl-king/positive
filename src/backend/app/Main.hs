{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Positive.CodeGen
import Positive.Flags
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
      logMsg_ logger (tshow flags)
      server logger flags
