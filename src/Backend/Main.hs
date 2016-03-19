{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Chat
import ClientSideGeneration
import qualified Network.EngineIO.Snap as EIOSnap
import qualified Snap.Core as Snap
import qualified Snap.Util.FileServe as Snap
import qualified Snap.Http.Server as Snap
import Snap.Http.Server.Config
import qualified Network.SocketIO as SocketIO
import qualified Snap.CORS as CORS
import Game.MapReader

--This is usefull for generating the file with ghci without having to start the server
writeGameStateElm :: IO ()
writeGameStateElm = writeFile "GameState.elm" generateGameState

main :: IO ()
main = do
  writeGameStateElm
  gameMap <- parseMap "test.dat"
  socketIoHandler <- SocketIO.initialize EIOSnap.snapAPI $ server gameMap
  Snap.httpServe (setPort 8001 defaultConfig) $ CORS.applyCORS CORS.defaultOptions $
    Snap.route [ ("/socket.io", socketIoHandler)
               , ("/", Snap.serveDirectory "../frontend")
               ]
