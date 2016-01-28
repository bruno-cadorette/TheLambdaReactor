{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Reactive (network) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Aeson
import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Aeson
import Debug.Trace
import Data.Text
import Network.SocketIO

data AddUser = AddUser Text deriving (Show)

instance FromJSON AddUser where
  parseJSON = withText "AddUser" $ pure . AddUser
  
  
data NewMessage = NewMessage Text deriving (Show)

instance FromJSON NewMessage where
  parseJSON = withText "NewMessage" $ pure . NewMessage

createCallback
  :: (FromJSON a, MonadIO m,
      MonadState RoutingTable m) =>
     Text -> m (AddHandler a)
createCallback text = do
    (addHandler, fire) <- liftIO $ registerCallback (const $ pure ())
    void $ text `on` fire
    return addHandler
    
registerCallback :: (MonadIO m, FromJSON a) => Handler a -> IO (AddHandler a, a -> m ())
registerCallback f = do
    (addHandler, fire) <- newAddHandler
    void $ register addHandler f
    return (addHandler, liftIO . fire)

socketIOEvent :: (MonadIO m, FromJSON a, MonadState RoutingTable m) => 
    Text -> m (MomentIO (Event a))
socketIOEvent = fmap fromAddHandler . createCallback


network :: (MonadIO m, MonadState RoutingTable m) => m ()
network = do
    newMessage <- socketIOEvent "sendMessage"
    addUser <- socketIOEvent "add user"
    n <- liftIO $ compile $ do
        x <- addUser
        y <- newMessage
        reactimate $ (\(AddUser t) -> print t) <$> x
        reactimate $ (\(NewMessage t) -> print t) <$> y
    liftIO $ actuate n