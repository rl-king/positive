module Positive.Flags where

import Options.Applicative
import Positive.Prelude
import Positive.Settings

-- FLAGS

data Flags = Flags
  { fDir :: !Dir,
    fIsDev :: !Bool
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
    <*> flag False True (long "dev")

parseDir :: Parser Dir
parseDir =
  Dir
    <$> strOption
      ( long "dir"
          <> short 'd'
          <> showDefault
          <> value "."
          <> metavar "DIR"
          <> help "Directory to use"
      )
