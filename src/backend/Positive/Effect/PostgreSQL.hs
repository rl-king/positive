{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Positive.Effect.PostgreSQL
  ( PostgreSQL (..),
    runSession,
    runTransaction,
    runPostgreSQL,
    Error (..),
  )
where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Effect.Throw
import qualified Hasql.Pool as Hasql
import Hasql.Session (Session)
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..))
import qualified Hasql.Transaction.Sessions as Transaction
import Positive.Prelude

data PostgreSQL (m :: Type -> Type) k where
  Session :: Session a -> PostgreSQL m (Either Error a)
  Transaction :: Transaction a -> PostgreSQL m (Either Error a)

newtype Error = Error Hasql.UsageError
  deriving (Show, Eq)

runTransaction ::
  (Has PostgreSQL sig m, Has (Throw Error) sig m) => Transaction a -> m a
runTransaction =
  liftEither <=< send . Transaction

runSession :: (Has PostgreSQL sig m, Has (Throw Error) sig m) => Session a -> m a
runSession =
  liftEither <=< send . Session

newtype PostgreSQLC m a = PostgreSQLC {unPostgreSQLC :: ReaderC Hasql.Pool m a}
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  (Algebra sig m, Has (Lift IO) sig m) =>
  Algebra (PostgreSQL :+: sig) (PostgreSQLC m)
  where
  alg hdl sig ctx =
    case sig of
      R other ->
        PostgreSQLC (alg (unPostgreSQLC . hdl) (R other) ctx)
      L (Session session) -> do
        pool <- PostgreSQLC ask
        (<$ ctx) . first Error <$> sendIO (Hasql.use pool session)
      L (Transaction transaction) -> do
        pool <- PostgreSQLC ask
        (<$ ctx) . first Error
          <$> sendIO
            ( Hasql.use pool $
                Transaction.transaction Serializable Write transaction
            )

runPostgreSQL :: Hasql.Pool -> PostgreSQLC m a -> m a
runPostgreSQL pool =
  runReader pool . unPostgreSQLC
