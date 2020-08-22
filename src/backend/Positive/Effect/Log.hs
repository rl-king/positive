{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Positive.Effect.Log where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Lift
import Positive.Prelude
import System.Log.FastLogger (TimedFastLogger, toLogStr)

data Log (m :: Type -> Type) k where
  Log :: Text -> Log m ()

log :: Has Log sig m => Text -> m ()
log =
  send . Log

newtype LogC m a = LogC {unFilmRollC :: ReaderC TimedFastLogger m a}
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, Has (Lift IO) sig m) => Algebra (Log :+: sig) (LogC m) where
  alg hdl sig ctx =
    case sig of
      R other ->
        LogC (alg (unFilmRollC . hdl) (R other) ctx)
      L (Log msg) -> do
        logger <- LogC ask
        (<$ ctx) <$> sendIO (log_ logger msg)

log_ :: TimedFastLogger -> Text -> IO ()
log_ logger msg =
  logger (\time -> toLogStr time <> " > " <> toLogStr msg <> "\n")

runLog :: TimedFastLogger -> LogC m a -> m a
runLog logger =
  runReader logger . unFilmRollC
