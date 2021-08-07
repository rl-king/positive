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
import qualified Data.Text as Text
import qualified Hasql.Pool as Hasql
import Network.Wai.EventSource
import qualified Network.Wai.Handler.Warp as Warp
import qualified Positive.CLI as CLI
import Positive.Effect.Log
import Positive.Effect.PostgreSQL
import qualified Positive.Effect.PostgreSQL as PostgreSQL
import qualified Positive.Metadata as Metadata
import Positive.Prelude
import Positive.Server.Api
import qualified Positive.Server.Handler as Handler
import qualified Positive.Static as Static
import Servant hiding (throwError)
import Servant.Server.Generic

-- SERVER

start :: TimedFastLogger -> Hasql.Pool -> CLI.IsDev -> CLI.Port -> IO ()
start logger pool isDev port =
  let settings =
        Warp.setPort port $
          Warp.setBeforeMainLoop
            ( putLogStr logger Info "server" $
                Text.concat
                  ["listening on port: ", tshow port, ", is dev: ", tshow isDev]
            )
            Warp.defaultSettings
   in do
        imageMVar <- MVar.newMVar Nothing
        previewMVar <- MVar.newMVar ()
        eventChan <- Chan.newChan
        let env = Handler.Env imageMVar previewMVar isDev
        _ <-
          forkIO $
            Metadata.loop previewMVar
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

handlers :: Handler.Handler sig m => CLI.IsDev -> Chan ServerEvent -> Api (AsServerT m)
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
              getCollections = Handler.handleGetCollections,
              addToCollection = Handler.handleAddToCollection,
              removeFromCollection = Handler.handleRemoveFromCollection,
              setCollectionTarget = Handler.handleSetCollectionTarget,
              getSettingsHistogram = Handler.handleGetSettingsHistogram,
              generateHighRes = Handler.handleGenerateHighRes,
              openExternalEditor = Handler.handleOpenExternalEditor,
              getCoordinateInfo = Handler.handleGetCoordinateInfo,
              generateWallpaper = Handler.handleGenerateWallpaper
            }
    }
