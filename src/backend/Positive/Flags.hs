{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Flags where

import Options.Applicative
import Positive.Prelude

-- FLAGS

data Flags = Flags
  { isDev :: !Bool,
    hard :: !Bool,
    mode :: !Mode
  }
  deriving (Show, Eq)

data Mode
  = Init
  | Previews
  | Contacts
  | SingleImage FilePath
  | Server
  deriving (Show, Eq)

parseArgs :: IO Flags
parseArgs =
  customExecParser (prefs showHelpOnError) $
    info (parser <**> helper) (fullDesc <> progDesc "positive")

parser :: Parser Flags
parser =
  Flags
    <$> flag False True (long "dev" <> help "generate elm, verbose logging")
    <*> flag False True (long "hard" <> help "reset settings")
    <*> ( flag' Init (long "init" <> short 'i')
            <|> flag' Previews (long "previews" <> short 'p')
            <|> flag' Contacts (long "contacts" <> short 'c')
            <|> SingleImage <$> strOption (long "single" <> short 's')
            <|> pure Server
        )
