{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
      flags@Flags {fDir, fCodeGen} <- parseArgs
      when fCodeGen (codeGen logger)
      logMsg_ logger (tshow flags)
      server logger fDir

-- FLAGS

data Flags = Flags
  { fDir :: !WorkingDirectory,
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

parseDir :: Parser WorkingDirectory
parseDir =
  WorkingDirectory
    <$> strOption
      ( long "dir"
          <> short 'd'
          <> showDefault
          <> value "./"
          <> metavar "DIR"
          <> help "Directory to use"
      )
