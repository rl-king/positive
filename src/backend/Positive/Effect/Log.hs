{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Positive.Effect.Log where

import Control.Algebra
import Control.Carrier.Reader
import qualified Control.Concurrent.Chan as Chan
import Control.Effect.Labelled
import Control.Effect.Lift
import qualified Data.ByteString.Builder as Builder
import Network.Wai.EventSource
import Positive.Prelude
import System.Log.FastLogger (TimedFastLogger, ToLogStr, toLogStr)

data Log (m :: Type -> Type) k where
  Log :: LogLevel -> Text -> Log m ()

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Bounded, Eq, Ord)

instance ToLogStr LogLevel where
  toLogStr = \case
    Debug -> "[debug]"
    Info -> "[info]"
    Warning -> "[warning]"
    Error -> "[error]"

logDebug,
  logInfo,
  logWarning,
  logError ::
    forall label sig m.
    HasLabelled (label :: Symbol) Log sig m =>
    Text ->
    m ()
logDebug = runUnderLabel @label . send . Log Debug
logInfo = runUnderLabel @label . send . Log Info
logWarning = runUnderLabel @label . send . Log Warning
logError = runUnderLabel @label . send . Log Error

-- STDOUT

newtype LogStdoutC m a = LogStdoutC
  { unLogStdout :: ReaderC TimedFastLogger m a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, Has (Lift IO) sig m) => Algebra (Log :+: sig) (LogStdoutC m) where
  alg hdl sig ctx =
    case sig of
      R other ->
        LogStdoutC (alg (unLogStdout . hdl) (R other) ctx)
      L (Log logLevel msg) -> do
        logger <- LogStdoutC ask
        (<$ ctx) <$> sendIO (putLogStr logger logLevel msg)

putLogStr :: TimedFastLogger -> LogLevel -> Text -> IO ()
putLogStr logger logLevel msg =
  logger (\t -> toLogStr t <> " " <> toLogStr logLevel <> " " <> toLogStr msg <> "\n")

runLogStdout :: TimedFastLogger -> LogStdoutC m a -> m a
runLogStdout logger =
  runReader logger . unLogStdout

-- SSE

newtype LogServerEventC m a = LogServerEventC
  { unLogServerEvent :: ReaderC (Chan ServerEvent) m a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, Has (Lift IO) sig m) => Algebra (Log :+: sig) (LogServerEventC m) where
  alg hdl sig ctx =
    case sig of
      R other ->
        LogServerEventC (alg (unLogServerEvent . hdl) (R other) ctx)
      L (Log _ msg) -> do
        eventChan <- LogServerEventC ask
        sendIO . Chan.writeChan eventChan $
          ServerEvent (Just "log") Nothing [Builder.byteString $ encodeUtf8 msg]
        pure (() <$ ctx)

runLogServerEvent :: Chan ServerEvent -> LogServerEventC m a -> m a
runLogServerEvent chan =
  runReader chan . unLogServerEvent
