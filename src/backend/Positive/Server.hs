{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Positive.Server
  ( run,
  )
where

import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Data.OrdPSQ as OrdPSQ
import qualified Data.Text as Text
import qualified Hasql.Pool
import Network.Wai.EventSource
import qualified Network.Wai.Handler.Warp as Warp
import qualified Positive.CLI as CLI
import qualified Positive.Log as Log
import Positive.Prelude hiding (ByteString)
import qualified Positive.Preview as Preview
import Positive.Server.Api
import qualified Positive.Server.Handler as Handler
import qualified Positive.Static as Static
import Servant
import Servant.Server.Generic

-- SERVER

run :: Log.TimedFastLogger -> CLI.IsDev -> CLI.Port -> IO ()
run logger isDev port =
  let settings =
        Warp.setPort port $
          Warp.setBeforeMainLoop
            ( Log.log logger $
                Text.concat
                  ["listening on port: ", tshow port, ", is dev: ", tshow isDev]
            )
            Warp.defaultSettings
   in do
        imageMVar <- MVar.newMVar OrdPSQ.empty
        previewMVar <- MVar.newMVar ()
        eventChan <- Chan.newChan
        pool <- Hasql.Pool.acquire (3, 10, "host=localhost port=5432 dbname=positive")
        let env = Handler.Env imageMVar previewMVar eventChan isDev pool logger
        _ <-
          forkIO $
            Preview.loop pool previewMVar eventChan (Log.log logger)
        Warp.runSettings settings $
          genericServeT (`runReaderT` env) (handlers isDev eventChan)

-- HANDLERS

handlers :: CLI.IsDev -> Chan ServerEvent -> Api (AsServerT (Handler.PositiveT Handler))
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
