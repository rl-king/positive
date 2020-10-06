{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Flags where

import Options.Applicative
import Positive.Prelude

-- FLAGS

data Flags = Flags
  { isDev :: !Bool,
    mode :: !Mode
  }
  deriving (Show, Eq)

data Mode
  = Init Replace
  | Previews Replace
  | Contacts
  | SingleImage FilePath
  | Server
  deriving (Show, Eq)

type Replace = Bool

parseArgs :: IO Flags
parseArgs =
  customExecParser (prefs showHelpOnError) $
    info (parser <**> helper) (fullDesc <> progDesc "positive")

parser :: Parser Flags
parser =
  Flags
    <$> flag False True (long "dev" <> help "generate elm, verbose logging")
      <*> ( flag' Init (long "init" <> short 'i' <> help "create image-settings.json and previews")
              <*> replace
                <|> flag' Previews (long "previews" <> short 'p' <> help "generate previews")
              <*> replace
                <|> flag' Contacts (long "contacts" <> short 'c' <> help "generate contactsheet")
                <|> SingleImage <$> strOption (long "single" <> short 's' <> help "generate single highres image")
                <|> pure Server
          )

replace :: Parser Replace
replace =
  flag False True (long "replace" <> help "replace existing")
