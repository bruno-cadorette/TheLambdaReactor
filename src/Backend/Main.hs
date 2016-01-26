{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Chat (server, ServerState (..),World (..),Message (..),PacketType(..))
import Control.Applicative

import qualified Network.EngineIO.Snap as EIOSnap
import qualified Data.Map.Strict as Map
import qualified Control.Concurrent.STM as STM
import qualified Snap.Core as Snap
import qualified Snap.Util.FileServe as Snap
import qualified Snap.Http.Server as Snap
import Snap.Http.Server.Config
import qualified Network.SocketIO as SocketIO
import qualified Snap.CORS as CORS
import qualified Data.Text as Text
import Debug.Trace

main :: IO ()
main = do
  state <- ServerState <$> STM.newTVarIO 0 <*> STM.newTVarIO (World [])
  socketIoHandler <- SocketIO.initialize EIOSnap.snapAPI (server state)
  Snap.httpServe (setPort 8001 defaultConfig) $ CORS.applyCORS CORS.defaultOptions $
    Snap.route [ ("/socket.io", socketIoHandler)
               , ("/", Snap.serveDirectory "../frontend")
               ]