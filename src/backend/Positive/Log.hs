module Positive.Log (TimedFastLogger, format, log) where

import Positive.Prelude
import System.Log.FastLogger (FormattedTime, LogStr, TimedFastLogger, toLogStr)

log :: TimedFastLogger -> Text -> IO ()
log logger msg =
  logger (format "info" msg)

format :: Text -> Text -> FormattedTime -> LogStr
format lvl msg time =
  toLogStr time <> " [" <> toLogStr lvl <> "] " <> toLogStr msg <> "\n"
