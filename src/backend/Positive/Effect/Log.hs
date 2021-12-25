{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
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
import System.Log.FastLogger (ToLogStr, toLogStr)


data Log (m :: Type -> Type) a where
  Log :: LogLevel -> Text -> Text -> Log m ()
  Context :: Text -> m a -> Log m a


data LogLevel
  = Trace
  | Error
  deriving (Bounded, Eq, Ord)


instance ToLogStr LogLevel where
  toLogStr = \case
    Trace -> "[trace]"
    Error -> "[error]"


logTrace
  , logError ::
    forall label sig m.
    HasLabelled (label :: Symbol) Log sig m =>
    Text ->
    Text ->
    m ()
logTrace context = runUnderLabel @label . send . Log Trace context
logError context = runUnderLabel @label . send . Log Error context


logTraceShow
  , logErrorShow ::
    forall label sig m msg.
    (Show msg, HasLabelled (label :: Symbol) Log sig m) =>
    Text ->
    msg ->
    m ()
logTraceShow context = logTrace @label context . tshow
logErrorShow context = logError @label context . tshow


withContext ::
  forall label sig m a.
  HasLabelled (label :: Symbol) Log sig m =>
  Text ->
  m a ->
  m a
withContext a m = sendLabelled @label $ Context a m


-- STDOUT

newtype LogStdoutC m a = LogStdoutC
  { unLogStdout :: ReaderC ([Text], TimedFastLogger) m a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO)


instance
  (Algebra sig m, Has (Lift IO) sig m) =>
  Algebra (Log :+: sig) (LogStdoutC m)
  where
  alg hdl sig ctx =
    case sig of
      R other ->
        LogStdoutC (alg (unLogStdout . hdl) (R other) ctx)
      L (Context context m) ->
        LogStdoutC $
          local @([Text], TimedFastLogger)
            (first (<> [context]))
            (unLogStdout (hdl (m <$ ctx)))
      L (Log logLevel title msg) -> do
        (context, logger) <- LogStdoutC ask
        (<$ ctx) <$> sendIO (putLogStr logger logLevel context title msg)


putLogStr :: TimedFastLogger -> LogLevel -> [Text] -> Text -> Text -> IO ()
putLogStr logger logLevel context title msg =
  logger
    ( \t ->
        mconcat
          [ toLogStr t
          , " "
          , toLogStr logLevel
          , " "
          , if null context
              then ""
              else toLogStr (Text.intercalate " | " context) <> " | "
          , toLogStr $ title <> " > "
          , toLogStr msg
          , "\n"
          ]
    )


runLogStdout :: TimedFastLogger -> LogStdoutC m a -> m a
runLogStdout logger =
  runReader ([], logger) . unLogStdout


-- SSE

newtype LogServerEventC m a = LogServerEventC
  { unLogServerEvent :: ReaderC ([Text], Chan ServerEvent) m a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO)


instance
  (Algebra sig m, Has (Lift IO) sig m) =>
  Algebra (Log :+: sig) (LogServerEventC m)
  where
  alg hdl sig ctx =
    case sig of
      R other ->
        LogServerEventC (alg (unLogServerEvent . hdl) (R other) ctx)
      L (Context context m) ->
        LogServerEventC $
          local @([Text], Chan ServerEvent)
            (first (<> [context]))
            (unLogServerEvent (hdl (m <$ ctx)))
      L (Log _ title msg) -> do
        (_context, eventChan) <- LogServerEventC (ask @([Text], Chan ServerEvent))
        sendIO . Chan.writeChan eventChan $
          ServerEvent
            (Just (Builder.byteString $ encodeUtf8 title))
            Nothing
            [Builder.byteString $ encodeUtf8 msg]
        pure (() <$ ctx)


runLogServerEvent :: Chan ServerEvent -> LogServerEventC m a -> m a
runLogServerEvent chan =
  runReader ([], chan) . unLogServerEvent
