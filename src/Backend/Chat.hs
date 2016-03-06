{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Chat (server) where

import Reactive
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import UserManagement
import Data.Text
import Message
import Network.SocketIO
import GameState
import GameStateManagement


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
            gameObject <- accumB emptyGameState $ fmap mergeGameState mix
            reactimate $ (toOutput . connectionMessageSender) <$> connectedPlayers <@> connectionEvent
            reactimate $ (toOutput . sendMessage) <$> connectedPlayers <@> sendMessageEvent
            reactimate $ (\ g s t -> toOutputMaybe (gameStateSender g t) s) <$> gameObject <*>  sockBehavior <@> fpsEvent

        actuate network
