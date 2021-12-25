{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Positive.Timed where

import Control.Effect.Labelled
import Control.Effect.Lift
import Control.Exception (evaluate)
import qualified Data.Time.Clock as Time
import Positive.Effect.Log
import Positive.Prelude


-- PROFILE

timedM :: (HasLabelled "stdout" Log sig m, Has (Lift IO) sig m) => Text -> m a -> m a
timedM name action = do
  logTrace @"stdout" "profiling started" name
  start <- sendIO Time.getCurrentTime
  a <- liftWith $ \run ctx -> evaluate =<< run (action <$ ctx)
  done <- sendIO Time.getCurrentTime
  logTraceShow @"stdout" (name <> " took") $ Time.diffUTCTime done start
  pure a


timed :: (HasLabelled "stdout" Log sig m, Has (Lift IO) sig m) => Text -> a -> m a
timed name action = do
  logTrace @"stdout" "profiling started" name
  start <- sendIO Time.getCurrentTime
  a <- sendIO $ evaluate action
  done <- sendIO Time.getCurrentTime
  logTraceShow @"stdout" (name <> " took") $ Time.diffUTCTime done start
  pure a
