{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}


module Chat (server) where

import Reactive
import qualified Data.Map.Strict as Map
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
import Data.Time
import Data.Maybe
import Debug.Trace


sendMessage :: PlayerNames -> (Socket, Text) -> EventHandler ()
sendMessage p (s, n) =
    case Map.lookup s p of
        Just x -> broadcastAll "receiveMessage" $ Message (pack (show x)) n
        Nothing -> broadcastAll "receiveMessage" $ Message "ERROR USER" n

server :: (MonadIO m, MonadState RoutingTable m) => m ()
server = do
    sendMessageSocket   <- createSocketEvent "sendMessage"
    usersSocket <- connectionManager
    inputSocket <- gameStateManager
    testSocket <- testManager

    liftIO $ do
        network <- compile $ do
            sendMessageEvent <- sendMessageSocket
            inputEvent <- testSocket
            (connectionEvent, connectedPlayers) <- usersSocket
            (fpsEvent,sockBehavior) <- fpsClock connectedPlayers
            gameStateObject <- inputSocket
            let mix = (updateStuff <$> connectedPlayers <*> gameStateObject) <@> inputEvent
            gameObject <- stepper emptyGameState mix
            reactimate $ (toOutput . connectionMessageSender) <$> connectedPlayers <@> connectionEvent
            reactimate $ (toOutput . sendMessage) <$> connectedPlayers <@> sendMessageEvent
            reactimate $ (toOutputMaybe . gameStateSender) <$> gameObject <*> sockBehavior <@> fpsEvent

        actuate network
