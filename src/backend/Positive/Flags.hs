module Positive.Flags where

import Options.Applicative
import Positive.Prelude

-- FLAGS

data Flags = Flags
  { fIsDev :: !Bool,
    fHard :: !Bool,
    fMode :: !Mode
  }
  deriving (Show, Eq)

data Mode
  = Init
  | Previews
  | Contacts
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
    <*> flag False True (long "hard")
    <*> ( flag' Init (long "init" <> short 'i')
            <|> flag' Previews (long "previews" <> short 'p')
            <|> flag' Contacts (long "contacts" <> short 'c')
            <|> pure Server
        )
