module Main where

import qualified Positive.CLI as CLI
import qualified Positive.Init as Init
import qualified Positive.Log as Log
import Positive.Prelude
import qualified Positive.Preview as Preview
import qualified Positive.Server as Server
import qualified Positive.SingleImage as SingleImage
import qualified System.Log.FastLogger as FastLogger

-- MAIN

main :: IO ()
main = do
  timeCache <- FastLogger.newTimeCache "%T"
  FastLogger.withTimedFastLogger timeCache (FastLogger.LogStdout FastLogger.defaultBufSize) $
    \logger -> do
      mode <- CLI.parseArgs
      let log = Log.log logger
      case mode of
        CLI.Init replace -> Init.run replace >> Preview.run log replace
        CLI.Previews replace -> Preview.run log replace
        CLI.SingleImage filepath -> SingleImage.run log filepath
        CLI.Server isDev port -> Server.run logger isDev port
