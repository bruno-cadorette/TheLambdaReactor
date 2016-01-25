{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}


module Chat (server, ServerState (..),World (..),Message (..),PacketType(..)) where

import Prelude hiding (mapM_)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Data.Aeson ((.=), (.:))
import Data.Foldable (mapM_)
import Debug.Trace
import Data.List
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Network.SocketIO as SocketIO
data Message = Message
    { method :: Text.Text
    , name :: Text.Text
    , body :: Text.Text
    } deriving (Generic,Show)

instance Aeson.ToJSON Message

instance Aeson.FromJSON Message where
  parseJSON  = Aeson.genericParseJSON Aeson.defaultOptions

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

decodeMessage :: Text.Text -> Maybe Message
decodeMessage text = Aeson.decode (BS.pack (Text.unpack text))

--server :: ServerState -> StateT SocketIO.RoutingTable Snap.Snap ()
server state = do
  userNameMVar <- liftIO STM.newEmptyTMVarIO
  let forUserName m = liftIO (STM.atomically (STM.tryReadTMVar userNameMVar)) >>= mapM_ m

  SocketIO.on "new message" $ \(NewMessage message) ->
    forUserName $ \userName ->
      SocketIO.broadcast "new message" (Said userName message)

  SocketIO.on "add user" $ \(AddUser text) ->let packet = Aeson.decode (BS.pack (Text.unpack text)) :: Maybe Message
                                              in
      case packet of
        Just (Message method userName body) -> do
                      n <- liftIO $ STM.atomically $ do
                        n <- (+ 1) <$> STM.readTVar (ssNConnected state)
                        STM.putTMVar userNameMVar userName
                        STM.writeTVar (ssNConnected state) n
                        return n

                      SocketIO.emit "login" (NumConnected n)
                      SocketIO.broadcast "login" (UserJoined userName n)
        Nothing -> SocketIO.emit "error" (Message (Text.pack "Error") (Text.pack "Couldn't parse Message on AddUser") text)
  --SocketIO.on "sendMessage" $ \(AddUser text) -> trace ("send " ++ (Text.unpack text)) $ return text

  SocketIO.on "sendMessage" $ \(AddUser text) ->
    trace ("send " ++ (Text.unpack text))
    $ forUserName $ \userName -> do
      case fmap (\ (Message method name body) -> (Said userName body)) $ decodeMessage text of
        Just x -> do 
            SocketIO.broadcast "receiveMessage" x
            SocketIO.emit "receiveMessage" x
        Nothing -> SocketIO.emit "error" (Message ( "Error") ("Couldn't parse Message on sendMessage") text)



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

  SocketIO.on "typing" $
    forUserName $ \userName ->
      SocketIO.emit "typing" (UserName userName)

  SocketIO.on "example2" $
    forUserName $ \userName ->
      SocketIO.emit "typing" (UserName userName)

  SocketIO.on "stop typing" $
    forUserName $ \userName ->
      SocketIO.broadcast "stop typing" (UserName userName)

  SocketIO.on "example" $ \ (Message method name body) ->
    forUserName $ \userName ->
      SocketIO.emit "example1" $ (Text.pack $ Debug.Trace.trace "allo" (show (6::Float)))
