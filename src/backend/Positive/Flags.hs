module Positive.Flags where

import Options.Applicative
import Positive.Prelude
import Positive.ImageSettings

-- FLAGS

data Flags = Flags
  { fDir :: !PathSegment,
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

parseDir :: Parser PathSegment
parseDir =
  PathSegment
    <$> strOption
      ( long "dir"
          <> short 'd'
          <> showDefault
          <> value "."
          <> metavar "DIR"
          <> help "Directory to use"
      )
