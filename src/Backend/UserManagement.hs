{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module UserManagement ( connectionMessageSender , PlayerNames,joinGame,leftGame,handleConnection) where

import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Reader
import Network.SocketIO
import Character
import Lib
import Game.MapReader
import Network.SocketIO.Reactive

type PlayerNames = Map.Map Socket Entity

--Handle connection event and apply the changes to the players map
handleConnection :: SocketInput ConnectionType -> PlayerNames -> PlayerNames
handleConnection (s, Connection _) m =  Map.insert s (Entity 100 (Location (V2 0.0 0.0) (V2 0.0 0.0))) m
handleConnection (s, Disconnection) m =  Map.delete s m

--TODO put the message in a ToJson instance so that the client will decide the message to show on each case
joinGame ::(MonadIO m, MonadReader Socket m) => String -> GameMap -> Socket -> m ()
joinGame n gameMap s  = do
                emit "login" (getSocketId s)
                emit "gameMap" gameMap
                broadcastAll "receiveServerMessage" (n `mappend` " has join the game")

leftGame :: (MonadIO m, MonadReader Socket m) => Socket -> m ()
leftGame s =
    broadcastAll "receiveServerMessage" ( getSocketId s `mappend` " has left the game")

connectionMessageSender :: (MonadIO m) => GameMap -> SocketInput ConnectionType -> ReaderT Socket m ()
connectionMessageSender gameMap (s,(Connection n)) = joinGame n gameMap s
connectionMessageSender _ (s,Disconnection) = leftGame s
