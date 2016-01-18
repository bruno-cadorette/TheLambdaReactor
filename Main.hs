{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Chat (server, ServerState (..))
import Control.Applicative

import qualified Network.EngineIO.Snap as EIOSnap
import qualified Control.Concurrent.STM as STM
import qualified Snap.Core as Snap
import qualified Snap.Util.FileServe as Snap
import qualified Snap.Http.Server as Snap
import Snap.Http.Server.Config
import qualified Network.SocketIO as SocketIO


main :: IO ()
main = do
  state <- ServerState <$> STM.newTVarIO 0
  socketIoHandler <- SocketIO.initialize EIOSnap.snapAPI (server state)
  Snap.httpServe (setPort 8001 defaultConfig) $
    Snap.route [ ("/socket.io", socketIoHandler)
               , ("/", Snap.serveDirectory "resources")
               ]
