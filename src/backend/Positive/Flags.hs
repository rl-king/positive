module Positive.Flags where

import Options.Applicative
import Positive.Prelude

-- FLAGS

data Flags = Flags
  { fIsDev :: !Bool,
    fInit :: !Bool
  }
  deriving (Show, Eq)

parseArgs :: IO Flags
parseArgs =
  customExecParser (prefs showHelpOnError) $
    info (parser <**> helper) (fullDesc <> progDesc "positive")

parser :: Parser Flags
parser =
  Flags <$> flag False True (long "dev") <*> flag False True (long "init")
