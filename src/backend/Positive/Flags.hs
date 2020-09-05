module Positive.Flags where

import Options.Applicative
import Positive.Prelude

-- FLAGS

data Flags = Flags
  { fIsDev :: !Bool,
    fMode :: !Mode
  }
  deriving (Show, Eq)

data Mode
  = Init
  | Previews
  | ContactSheet
  | Server
  deriving (Show, Eq)

parseArgs :: IO Flags
parseArgs =
  customExecParser (prefs showHelpOnError) $
    info (parser <**> helper) (fullDesc <> progDesc "positive")

parser :: Parser Flags
parser =
  Flags
    <$> flag False True (long "dev")
    <*> ( flag' Init (long "init" <> short 'i')
            <|> flag' Previews (long "previews" <> short 'p')
            <|> flag' ContactSheet (long "contactsheet" <> short 'c')
            <|> pure Server
        )
