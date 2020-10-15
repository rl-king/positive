module Positive.Log (TimedFastLogger, format, log) where

import Positive.Prelude
import System.Log.FastLogger (FormattedTime, LogStr, TimedFastLogger, toLogStr)

log :: MonadIO m => TimedFastLogger -> Text -> m ()
log logger =
  liftIO . logger . format "info"

format :: Text -> Text -> FormattedTime -> LogStr
format lvl msg time =
  toLogStr time <> " [" <> toLogStr lvl <> "] " <> toLogStr msg <> "\n"
