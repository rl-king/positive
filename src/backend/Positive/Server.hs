{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Server
  ( start,
  )
where

import Control.Carrier.Error.Church as Error.Church
import Control.Carrier.Error.Either as Error.Either
import Control.Carrier.Reader
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import Control.Effect.Labelled
import Control.Effect.Lift
import qualified Data.OrdPSQ as OrdPSQ
import qualified Data.Text as Text
import qualified Hasql.Pool
import Network.Wai.EventSource
import qualified Network.Wai.Handler.Warp as Warp
import qualified Positive.CLI as CLI
import Positive.Effect.Log
import Positive.Effect.PostgreSQL
import qualified Positive.Effect.PostgreSQL as PostgreSQL
import Positive.Prelude
import qualified Positive.Preview as Preview
import Positive.Server.Api
import qualified Positive.Server.Handler as Handler
import qualified Positive.Static as Static
import Servant hiding (throwError)
import Servant.Server.Generic
import System.Log.FastLogger (TimedFastLogger)

-- SERVER

start :: TimedFastLogger -> CLI.IsDev -> CLI.Port -> IO ()
start logger isDev port =
  let settings =
        Warp.setPort port $
          Warp.setBeforeMainLoop
            ( putLogStr logger Info "server" $
                Text.concat
                  ["listening on port: ", tshow port, ", is dev: ", tshow isDev]
            )
            Warp.defaultSettings
   in do
        imageMVar <- MVar.newMVar OrdPSQ.empty
        previewMVar <- MVar.newMVar ()
        eventChan <- Chan.newChan
        pool <- Hasql.Pool.acquire (3, 10, "host=localhost port=5432 dbname=positive")
        let env = Handler.Env imageMVar previewMVar isDev
        _ <-
          forkIO $
            Preview.loop previewMVar
              & runLabelled @"sse"
              & runLogServerEvent eventChan
              & runPostgreSQL pool
              & Error.Church.runError @PostgreSQL.Error
                (logError @"stdout" "preview" . tshow)
                pure
              & runLabelled @"stdout"
              & runLogStdout logger
        Warp.runSettings settings $
          genericServeT
            ( runLabelled @"sse"
                >>> runLogServerEvent eventChan
                >>> runReader env
                >>> runPostgreSQL pool
                >>> Error.Church.runError @PostgreSQL.Error
                  (\err -> logError @"stdout" "postgresql" (tshow err) >> throwError err500)
                  pure
                >>> Error.Either.runError
                >>> runLabelled @"stdout"
                >>> runLogStdout logger
                >>> ExceptT
                >>> Servant.Handler
            )
            (handlers isDev eventChan)

-- HANDLERS

handlers ::
  ( HasLabelled "stdout" Log sig m,
    HasLabelled "sse" Log sig m,
    Has PostgreSQL sig m,
    Has (Reader Handler.Env) sig m,
    Has (Lift IO) sig m,
    Has (Throw PostgreSQL.Error) sig m,
    Has (Throw ServerError) sig m
  ) =>
  CLI.IsDev ->
  Chan ServerEvent ->
  Api (AsServerT m)
handlers isDev chan =
  Api
    { imageApi =
        genericServerT
          ImageApi
            { image = Handler.handleImage,
              events = pure $ eventSourceAppChan chan,
              raw = Static.serve isDev
            },
      settingsApi =
        genericServerT
          SettingsApi
            { saveFilmRoll = Handler.handleSaveFilmRoll,
              checkExpressions = Handler.handleCheckExpressions,
              getSettings = Handler.handleGetSettings,
              getSettingsHistogram = Handler.handleGetSettingsHistogram,
              generateHighRes = Handler.handleGenerateHighRes,
              openExternalEditor = Handler.handleOpenExternalEditor,
              getCoordinateInfo = Handler.handleGetCoordinateInfo,
              generateWallpaper = Handler.handleGenerateWallpaper
            }
    }
