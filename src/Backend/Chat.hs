{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}


module Chat (server, ServerState (..),World (..),Message (..),PacketType(..)) where

import Prelude hiding (mapM_)
import Control.Monad.IO.Class
import Control.Applicative
import Data.Aeson ((.=), (.:))
import Data.Foldable (mapM_)
import Debug.Trace
import Data.List
import Control.Monad
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics
import Control.Monad.Reader.Class

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Network.SocketIO as SocketIO
import Message (Message (..),decodeMessage)
import Control.Event.Handler

data PacketType = Emit | Broadcast

data AddUser = AddUser Text.Text

instance Aeson.FromJSON AddUser where
  parseJSON = Aeson.withText "AddUser" $ pure . AddUser


data NumConnected = NumConnected !Int

instance Aeson.ToJSON NumConnected where
  toJSON (NumConnected n) = Aeson.object [ "numUsers" .= n]


data NewMessage = NewMessage Text.Text

instance Aeson.FromJSON NewMessage where
  parseJSON = Aeson.withText "NewMessage" $ pure . NewMessage


data Said = Said Text.Text Text.Text

instance Aeson.ToJSON Said where
  toJSON (Said username message) = Aeson.object
    [ "username" .= username
    , "message" .= message
    ]

data UserName = UserName Text.Text

instance Aeson.ToJSON UserName where
  toJSON (UserName un) = Aeson.object [ "username" .= un ]


data UserJoined = UserJoined Text.Text Int

instance Aeson.ToJSON UserJoined where
  toJSON (UserJoined un n) = Aeson.object
    [ "username" .= un
    , "numUsers" .= n
    ]

data World = World [Int]

--------------------------------------------------------------------------------
data ServerState = ServerState { ssNConnected :: STM.TVar Int, world :: STM.TVar World}

registerCallback :: (MonadIO m, Aeson.FromJSON a) => (a -> SocketIO.EventHandler b) -> IO (a -> m ())
registerCallback f = do
    (addHandler, fire) <- newAddHandler
    register addHandler (void . return . f)
    return $ liftIO . fire
   
forUserName :: MonadIO m => STM.TMVar a -> (a -> m b) -> m ()
forUserName userNameMVar m = liftIO (STM.atomically (STM.tryReadTMVar userNameMVar)) >>= mapM_ m   

onNewMessage :: STM.TMVar Text.Text -> NewMessage -> SocketIO.EventHandler ()
onNewMessage userNameMVar = (\(NewMessage message) -> forUserName userNameMVar $ (\userName -> SocketIO.broadcast "new message" (Said userName message)))
 
   
--server :: ServerState -> StateT SocketIO.RoutingTable Snap.Snap ()
server state = do
  userNameMVar <- liftIO STM.newEmptyTMVarIO
  onNewMessage <- liftIO $ registerCallback $ onNewMessage userNameMVar
  
  SocketIO.on "new message" $ onNewMessage
    
    
    
  SocketIO.on "add user" $ \(AddUser text) ->
      case decodeMessage text of
        Just (Message userName body) -> do
                      n <- liftIO $ STM.atomically $ do
                        n <- (+ 1) <$> STM.readTVar (ssNConnected state)
                        STM.putTMVar userNameMVar userName
                        STM.writeTVar (ssNConnected state) n
                        return n

                      SocketIO.emit "login" (NumConnected n)
                      SocketIO.broadcast "login" (UserJoined userName n)
        Nothing -> SocketIO.emit "error" (Message "Couldn't parse Message on AddUser " text)
  --SocketIO.on "sendMessage" $ \(AddUser text) -> trace ("send " ++ (Text.unpack text)) $ return text

  SocketIO.on "sendMessage" $ \(AddUser text) ->
    trace ("send " ++ (Text.unpack text))
    $ forUserName userNameMVar $ \userName -> do
      case decodeMessage text of
        Just x -> do 
            SocketIO.broadcast "receiveMessage" x
            SocketIO.emit "receiveMessage" x
        Nothing -> SocketIO.emit "error" (Message  "Couldn't parse Message on sendMessage"  text)



  SocketIO.appendDisconnectHandler $ do
    (n, mUserName) <- liftIO $ STM.atomically $ do
      n <- (+ (-1)) <$> STM.readTVar (ssNConnected state)
      mUserName <- STM.tryReadTMVar userNameMVar
      STM.writeTVar (ssNConnected state) n
      return (n, mUserName)

    case mUserName of
      Nothing -> return ()
      Just userName ->
        SocketIO.broadcast "user left" (UserJoined userName n)