{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}


module Chat (server) where

import Reactive
import qualified Data.Map as Map
import Control.Monad.State.Strict
import UserManagement
import Data.Aeson
import Data.Text
import qualified Data.Aeson as Aeson
import Message
import Network.SocketIO
import GameState
import GameStateManagement
import GameEngine
import Data.Time.Clock
import Data.Maybe

sendMessage :: PlayerNames -> (Socket, Text) -> EventHandler ()
sendMessage p (s, n) =
    case Map.lookup s p of
        Just x -> broadcastAll "receiveMessage" $ Message (pack x) n
        Nothing -> broadcastAll "receiveMessage" $ Message "ERROR USER" n

server :: (MonadIO m, MonadState RoutingTable m) => m ()
server = do
    sendMessageSocket   <- createSocketEvent "sendMessage"
    usersSocket <- connectionManager
    inputSocket <- gameStateManager

    liftIO $ do
        network <- compile $ do
            sendMessageEvent <- sendMessageSocket
            (connectionEvent, connectedPlayers) <- usersSocket
            (fpsEvent,sockBehavior) <- fpsClock connectedPlayers
            let regFps = whenE ((\x -> isJust x) <$> sockBehavior) fpsEvent
            gameStateObject <- inputSocket

            reactimate $ (toOutput . connectionMessageSender) <$> connectedPlayers <@> connectionEvent
            reactimate $ (toOutput . sendMessage) <$> connectedPlayers <@> sendMessageEvent
            reactimate $ (toOutputTime . gameStateSender) <$> gameStateObject <*> sockBehavior <@> regFps

        actuate network
