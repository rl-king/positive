module Positive.CLI where

import Options.Applicative
import Positive.Prelude


-- CLI

data Mode
  = Init
  | SingleImage FilePath
  | Server IsDev Port
  deriving (Show, Eq)


type IsDev = Bool


type Port = Int


parseArgs :: IO Mode
parseArgs =
  customExecParser (prefs showHelpOnError) $
    info (parser <**> helper) (fullDesc <> progDesc "positive")


parser :: Parser Mode
parser =
  asum
    [ flag'
        Init
        ( long "init"
            <> short 'i'
            <> help "add directory to database and generate previews"
        )
    , SingleImage
        <$> strOption
          ( long "single"
              <> short 's'
              <> metavar "FILENAME"
              <> help "process fullsize image to ./highres"
          )
    , Server <$> isDev <*> port
    ]


isDev :: Parser IsDev
isDev =
  flag False True (long "dev" <> help "generate elm, verbose logging")


port :: Parser Int
port =
  option auto $
    long "listen" <> short 'l' <> showDefault <> value 8080 <> metavar "INT"
      <> help "Port at which the server is run"
