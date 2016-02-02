{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Reactive (
    createSocketEvent, 
    disconnectEvent, 
    toOutput, 
    broadcastAll, 
    foldp, 
    GetSocket(..),
    module Reactive.Banana, 
    module Reactive.Banana.Frameworks) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Aeson
import Data.Text
import Network.SocketIO
  
class GetSocket a where
    getSocket :: a -> Socket

instance GetSocket (Socket, a) where
    getSocket = fst
    
instance GetSocket Socket where
    getSocket = id 
    

    
handler :: Handler (Socket, a) -> a -> ReaderT Socket IO ()
handler f x = ReaderT (\r -> f (r, x))
  
{-createCallbackInner
  :: (FromJSON a, MonadIO m,
      MonadState RoutingTable m) =>
     Text -> m (AddHandler (Socket, a))-}
createCallbackInner callback = do
    (addHandler, fire) <- liftIO $ registerCallback
    void $ callback fire
    return addHandler
    
createCallback
  :: (FromJSON a, MonadIO m,
      MonadState RoutingTable m) =>
     Text -> m (AddHandler (Socket, a))
createCallback text = createCallbackInner (on text . handler)

disconnectCallback :: (MonadIO m, MonadState RoutingTable m) => m (AddHandler (Socket, ()))
disconnectCallback = createCallbackInner (\fire -> appendDisconnectHandler $ handler fire ())
    
registerCallback :: (MonadIO m, FromJSON a) => IO (AddHandler (Socket, a), (Socket, a) -> m ())
registerCallback = do
    (addHandler, fire) <- newAddHandler
    void $ register addHandler (const $ pure ())
    return (addHandler, liftIO . fire)

createSocketEvent :: (MonadIO m, FromJSON a, MonadState RoutingTable m) => 
    Text -> m (MomentIO (Event (Socket, a)))
createSocketEvent = fmap fromAddHandler . createCallback 

disconnectEvent :: (MonadIO m, MonadState RoutingTable m) => m (MomentIO (Event (Socket, ())))
disconnectEvent = fmap fromAddHandler disconnectCallback

broadcastAll :: (MonadIO m, MonadReader Socket m, ToJSON a) => Text -> a -> m ()
broadcastAll text x = do
    emit text x
    broadcast text x
    
toOutput :: GetSocket s => (s -> ReaderT Socket m a) -> s -> m a
toOutput event a = runReaderT (event a) $ getSocket a

foldp :: MonadMoment m => (a -> b -> b) -> b -> Event a -> m (Event b)
foldp f z e = accumE z (fmap f e)