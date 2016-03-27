{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module UserManagement (connectionManager, connectionMessageSender, UserConnection(..), PlayerNames) where

import Reactive
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Reader
import Network.SocketIO
import Character
import Lib
import Game.MapReader
import Debug.Trace

type Username = String
type PlayerNames = Map.Map Socket Entity

data UserConnection = EnterGame Username Socket | LeaveGame Socket | Both Username Socket Socket
instance GetSocket UserConnection where
    getSocket (EnterGame _ s) = s
    getSocket (LeaveGame s) = s
    getSocket (Both _ s _) = s


connectionManager :: (MonadIO m, MonadState RoutingTable m) => m (MomentIO (Event UserConnection, Behavior PlayerNames))
connectionManager = do
    userConnectedSocket <- createSocketEvent "newUser"
    disconnectSocket    <- disconnectEvent
    return $ do
        userConnected  <-  userConnectedSocket
        userDisconnect <- disconnectSocket

        let connectionEvent = (\(s, n) -> EnterGame n s) <$> userConnected
        let quittingEvent = (\(s, _) -> LeaveGame s) <$> userDisconnect

        mapAccum Map.empty $ fmap connection $ unionWith (\(EnterGame n s) (LeaveGame s') -> Both n s s') connectionEvent quittingEvent
    where
        connection (EnterGame n s) m = ((EnterGame n s), Map.insert s (Entity 100 (Location (V2 0.0 0.0) (V2 0.0 0.0))) (trace (show $ Map.elems m) m))
        connection (LeaveGame s) m = ((LeaveGame s), Map.delete s m)
        connection (Both n s s') m = ((Both n s s'), Map.insert s (Entity 100 (Location (V2 0.0 0.0) (V2 0.0 0.0))) $ Map.delete s' m)

--TODO put the message in a ToJson instance so that the client will decide the message to show on each case
joinGame ::(MonadIO m, MonadReader Socket m) => String -> GameMap -> Socket -> m ()
joinGame n gameMap s  = do
                emit "login" (getSocketId s)
                emit "gameMap" gameMap
                broadcastAll "receiveServerMessage" (n `mappend` " has join the game")
--MonadIO m MonadReader Socket m
leftGame :: (MonadIO m, MonadReader Socket m) => Socket -> Map.Map Socket a -> m ()
leftGame s m =
    case Map.lookup s m of
        Just _ -> broadcastAll "receiveServerMessage" ( (getSocketId s) `mappend` " has left the game")
        Nothing -> broadcastAll "receiveServerMessage" ("Someone has left the game" :: String) -- return () -- TODO log

connectionMessageSender :: (MonadIO m) => GameMap -> PlayerNames -> UserConnection -> ReaderT Socket m ()
connectionMessageSender gameMap _ (EnterGame n s) = joinGame n gameMap s
connectionMessageSender _ m (LeaveGame s) = leftGame s m
connectionMessageSender gameMap m (Both n _ s) = do
    joinGame n gameMap s
    leftGame s m
