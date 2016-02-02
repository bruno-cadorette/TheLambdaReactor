{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}


module Chat (server, World (..),Message (..),PacketType(..)) where

import Reactive 
import Control.Monad.State.Strict
import UserManagement
import Data.Aeson
import Data.Text
import Message
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Network.SocketIO

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

data World = World [Int]

--------------------------------------------------------------------------------




        
sendMessage :: (a, Text) -> EventHandler ()
sendMessage  = broadcastAll "receiveMessage" . snd


server :: (MonadIO m, MonadState RoutingTable m) => m ()
server = do
    sendMessageSocket   <- createSocketEvent "sendMessage"
    usersSocket <- connectionManager
    liftIO $ do
        network <- compile $ do
            sendMessageEvent <- sendMessageSocket
            (connectionEvent, connectedPlayers) <- usersSocket
            reactimate $ (\n -> toOutput (connectionMessageSender n)) <$> connectedPlayers <@> connectionEvent
            reactimate $ (toOutput sendMessage) <$> sendMessageEvent
        actuate network