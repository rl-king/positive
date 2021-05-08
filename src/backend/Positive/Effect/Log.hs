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
import qualified Data.Text as Text
import Network.Wai.EventSource
import Positive.Prelude
import System.Log.FastLogger (TimedFastLogger, toLogStr)

data Log (m :: Type -> Type) k where
  Log :: LogLevel -> Text -> Text -> Log m ()

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Bounded, Eq, Ord)

logLevelToText :: LogLevel -> Text
logLevelToText = \case
  Debug -> "[debug]"
  Info -> "[info]"
  Warning -> "[warn]"
  Error -> "[error]"

logDebug,
  logInfo,
  logWarning,
  logError ::
    forall label sig m.
    HasLabelled (label :: Symbol) Log sig m =>
    Text ->
    Text ->
    m ()
logDebug context = runUnderLabel @label . send . Log Debug context
logInfo context = runUnderLabel @label . send . Log Info context
logWarning context = runUnderLabel @label . send . Log Warning context
logError context = runUnderLabel @label . send . Log Error context

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
      L (Log logLevel context msg) -> do
        logger <- LogStdoutC ask
        (<$ ctx) <$> sendIO (putLogStr logger logLevel context msg)

putLogStr :: TimedFastLogger -> LogLevel -> Text -> Text -> IO ()
putLogStr logger logLevel context msg =
  logger
    ( \t ->
        mconcat
          [ toLogStr t,
            " ",
            toLogStr (Text.justifyLeft 8 ' ' (logLevelToText logLevel)),
            toLogStr ("<" <> context <> ">"),
            toLogStr msg,
            "\n"
          ]
    )

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
      L (Log _ context msg) -> do
        eventChan <- LogServerEventC ask
        sendIO . Chan.writeChan eventChan $
          ServerEvent
            (Just (Builder.byteString $ encodeUtf8 context))
            Nothing
            [Builder.byteString $ encodeUtf8 msg]
        pure (() <$ ctx)

runLogServerEvent :: Chan ServerEvent -> LogServerEventC m a -> m a
runLogServerEvent chan =
  runReader chan . unLogServerEvent
