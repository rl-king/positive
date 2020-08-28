module Positive.Flags where

import qualified Data.Text as Text
import Options.Applicative
import Positive.Prelude
import Positive.Settings
import System.FilePath.Posix (dropTrailingPathSeparator)

-- FLAGS

data Flags = Flags
  { fDir :: !WorkingDirectory,
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

parseDir :: Parser WorkingDirectory
parseDir =
  WorkingDirectory . Text.pack . dropTrailingPathSeparator
    <$> strOption
      ( long "dir"
          <> short 'd'
          <> showDefault
          <> value "."
          <> metavar "DIR"
          <> help "Directory to use"
      )
