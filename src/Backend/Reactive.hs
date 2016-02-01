{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Reactive (createSocketEvent, toOutput) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Aeson
import Debug.Trace
import Data.Text
import Network.SocketIO
import Message
  
handler :: ((Socket, a) -> IO ()) -> a -> ReaderT Socket IO ()
handler f x = ReaderT (\r -> f (r, x))
  
createCallback
  :: (FromJSON a, MonadIO m,
      MonadState RoutingTable m) =>
     Text -> m (AddHandler (Socket, a))
createCallback text = do
    (addHandler, fire) <- liftIO $ registerCallback
    void $ text `on` (trace "handler" . handler fire)
    return addHandler
    
registerCallback :: (MonadIO m, FromJSON a) => IO (AddHandler (Socket, a), (Socket, a) -> m ())
registerCallback = do
    (addHandler, fire) <- newAddHandler
    void $ register addHandler (const $ pure ())
    return (addHandler, liftIO . fire)

createSocketEvent :: (MonadIO m, FromJSON a, MonadState RoutingTable m) => 
    Text -> m (MomentIO (Event (Socket, a)))
createSocketEvent = fmap fromAddHandler . createCallback 



broadcastAll :: (MonadIO m, MonadReader Socket m, ToJSON a) => Text -> a -> m ()
broadcastAll text x = do
    emit text x
    broadcast text x

sendMessage :: Text -> EventHandler ()
sendMessage  = broadcastAll "receiveMessage"

toOutput :: (t -> ReaderT r m a) -> (r, t) -> m a
toOutput event (r, a) = runReaderT (event a) r

    
network :: (MonadIO m, MonadState RoutingTable m) => m ()
network =  do
    newMessage <- createSocketEvent "sendMessage"
    liftIO $ do
        n <- compile $ do
            x <-  newMessage
            reactimate $ (toOutput sendMessage) <$> x
        actuate n 