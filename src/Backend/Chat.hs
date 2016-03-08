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
import Game.MapReader
import Game.Map
import Debug.Trace
import Lib

sendMessage :: PlayerNames -> (Socket, Text) -> EventHandler ()
sendMessage p (s, n) =
    case Map.lookup s p of
        Just x -> broadcastAll "receiveMessage" $ Message (pack (show x)) n
        Nothing -> broadcastAll "receiveMessage" $ Message "ERROR USER" n

--setGameEvent :: m (Behavior GameEngine) -> Behavior (Map Socket Entity) -> Event UserInput -> Event a -> m (Behavior GameState)
setGameEvent inputSocket connectedPlayers inputEvent fpsEvent mapBound = do
  gameStateObject <- inputSocket
  let mix = (updateStuff <$> connectedPlayers <*> gameStateObject) <@> inputEvent
  gameObject <- accumB emptyGameState $ fmap mergeGameState mix
  gameUpdated <- accumB  emptyGameState $((\ updates time old -> (moveAllPlayer mapBound) $ mergeGameState updates old ) <$> gameObject <@> fpsEvent)
  return gameUpdated

server :: (MonadIO m, MonadState RoutingTable m) =>  Map.Map (V2 Float) Int -> m ()
server gameMap =let mapBound = createMap $ Map.foldrWithKey (\ k x acc -> if x == 1 then k : acc else acc ) [] gameMap
                 in do
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
            x <- setGameEvent inputSocket connectedPlayers inputEvent fpsEvent mapBound
            reactimate $ (toOutput . (connectionMessageSender $mapToExport gameMap)) <$> connectedPlayers <@> connectionEvent
            reactimate $ (toOutput . sendMessage) <$> connectedPlayers <@> sendMessageEvent
            reactimate $ (\ g s t -> toOutputMaybe (gameStateSender g t) s) <$> x <*>  sockBehavior <@> fpsEvent

        actuate network
