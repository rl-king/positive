{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Positive.Effect.Profile where

import Control.DeepSeq
import Control.Effect.Labelled
import Control.Effect.Lift
import Control.Exception (evaluate)
import qualified Data.Time.Clock as Time
import Positive.Effect.Log
import Positive.Prelude

data Profile (m :: Type -> Type) k where
  Profile :: NFData a => Text -> a -> Profile m a

measure :: (Has Profile sig m, NFData a) => Text -> a -> m a
measure name =
  send . Profile name

newtype ProfileC m a = ProfileC {unProfileC :: m a}
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, Has (Lift IO) sig m, HasLabelled "stdout" Log sig m) => Algebra (Profile :+: sig) (ProfileC m) where
  alg hdl sig ctx =
    case sig of
      R other -> ProfileC (alg (unProfileC . hdl) other ctx)
      L (Profile name m) -> do
        start <- sendIO Time.getCurrentTime
        a <- sendIO . evaluate $ force m
        done <- sendIO Time.getCurrentTime
        logInfo @"stdout" "profile" $ name <> " - took: " <> tshow (Time.diffUTCTime done start)
        pure $ a <$ ctx

runProfile :: ProfileC m a -> m a
runProfile =
  unProfileC
