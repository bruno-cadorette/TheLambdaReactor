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

data AddUser = AddUser Text

instance Aeson.FromJSON AddUser where
  parseJSON = Aeson.withText "AddUser" $ pure . AddUser


data NumConnected = NumConnected !Int

instance Aeson.ToJSON NumConnected where
  toJSON (NumConnected n) = Aeson.object [ "numUsers" .= n]


data NewMessage = NewMessage Text

instance Aeson.FromJSON NewMessage where
  parseJSON = Aeson.withText "NewMessage" $ pure . NewMessage


data Said = Said Text Text

instance Aeson.ToJSON Said where
  toJSON (Said username message) = Aeson.object
    [ "username" .= username
    , "message" .= message
    ]

data UserName = UserName Text

instance Aeson.ToJSON UserName where
  toJSON (UserName un) = Aeson.object [ "username" .= un ]


data UserJoined = UserJoined Text Int

instance Aeson.ToJSON UserJoined where
  toJSON (UserJoined un n) = Aeson.object
    [ "username" .= un
    , "numUsers" .= n
    ]

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
            (inputEvent,gameStateObject) <- inputSocket
            reactimate $ (toOutput . connectionMessageSender) <$> connectedPlayers <@> connectionEvent
            reactimate $ (toOutput . sendMessage) <$> connectedPlayers <@> sendMessageEvent
            reactimate $ (toOutput . gameStateSender) <$> gameStateObject <@> inputEvent
        actuate network
