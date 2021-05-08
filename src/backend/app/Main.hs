module Main where

import qualified Hasql.Pool
import qualified Positive.CLI as CLI
import qualified Positive.Init as Init
import Positive.Prelude
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
      pool <- Hasql.Pool.acquire (3, 10, "host=localhost port=5432 dbname=positive")
      case mode of
        CLI.Init replace -> Init.run replace
        CLI.SingleImage filepath -> SingleImage.run logger pool filepath
        CLI.Server isDev port -> Server.start logger pool isDev port
