{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (when)
import Options.Applicative
import Positive.CodeGen
import Positive.Server
import Positive.Settings
import qualified System.Log.FastLogger as FastLogger

-- MAIN

main :: IO ()
main = do
  timeCache <- FastLogger.newTimeCache FastLogger.simpleTimeFormat
  FastLogger.withTimedFastLogger timeCache (FastLogger.LogStdout FastLogger.defaultBufSize) $
    \logger -> do
      Flags {fDir, fCodeGen} <- parseArgs
      when fCodeGen (codeGen logger)
      server logger fDir

-- FLAGS

data Flags = Flags
  { fDir :: !Dir,
    fCodeGen :: !Bool
  }
  deriving (Show, Eq)

parseArgs :: IO Flags
parseArgs =
  customExecParser (prefs showHelpOnError) $
    info (parser <**> helper) (fullDesc <> progDesc "positive")

parser :: Parser Flags
parser =
  Flags
    <$> parseDir
    <*> flag False True (long "codegen")

parseDir :: Parser Dir
parseDir =
  Dir
    <$> option
      auto
      ( long "dir"
          <> short 'd'
          <> showDefault
          <> value "./"
          <> metavar "DIR"
          <> help "Directory to use"
      )
