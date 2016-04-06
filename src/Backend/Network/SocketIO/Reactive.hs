{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Network.SocketIO.Reactive(
    GetSocket(..),
    SocketInput,
    broadcastAll,
    SocketListener(..),
    reactimateSocket,
    initializeWithReactive,
    module Reactive.Banana,
    module Reactive.Banana.Frameworks) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Concurrent
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Aeson
import Pipes.Concurrent
import Pipes
import Data.Text (Text)
import Data.Monoid
import Data.ByteString.Lazy.Char8 (pack)
import Network.SocketIO
import Network.EngineIO (ServerAPI)


class GetSocket a where
    getSocket :: a -> Socket

instance GetSocket (SocketInput a) where
    getSocket = fst

instance GetSocket Socket where
    getSocket = id
    
    
type SocketInput a = (Socket, a)

data SocketListener b where
    OnListen :: FromJSON a => Text -> (a -> b) -> SocketListener b -- ^ This is like the 'on' function on the socket-io package
    OnDisconnect ::  b -> SocketListener b -- ^ This is like the 'appendDisconnectHandler' function on the socket-io package

handler :: (FromJSON a) => (SocketInput a -> IO ()) -> a -> ReaderT Socket IO ()
handler f x = ReaderT (\r -> f (r, x))    

registerCallback :: (MonadIO m) => IO (AddHandler (SocketInput a), (SocketInput a) -> m ())
registerCallback = do
    (addHandler, fire) <- newAddHandler
    void $ register addHandler (const $ pure ())
    return (addHandler, liftIO . fire)

{- Broadcast to every socket, including the current socket -}
broadcastAll :: (MonadIO m, MonadReader Socket m, ToJSON a) => Text -> a -> m ()
broadcastAll text x = do
    emit text x
    broadcast text x

{- Like the reactimate function from Reactive.Banana, but for Socket Event instead of just IO () -}
reactimateSocket :: GetSocket s => (s -> ReaderT Socket IO ()) -> Event s -> MomentIO ()
reactimateSocket f = reactimate . fmap (\a -> runReaderT (f a) $ getSocket a)
    
    
fireCallback :: Handler (SocketInput a) -> Consumer (SocketInput a) IO ()
fireCallback fire = forever $ await >>= liftIO . fire    
    
runListener :: MonadState RoutingTable m => Output (SocketInput t) -> SocketListener t -> m ()
runListener output (OnListen text f) =
    on text (handler (\(s, a) -> 
        case decode $ pack a of
            Just v  -> do
                print $ "Received " <> show a
                void $ atomically $ send output (s, f v)
            Nothing -> do 
                print $ "Decode error on " <> text
                print $ "Received " <> show a))
runListener output (OnDisconnect a) = 
    appendDisconnectHandler (handler (\(s, ()) -> void $ atomically $ send output (s, a)) ())
    
mergeListeners
  :: (MonadState RoutingTable m) =>
     Output (SocketInput a) -> [SocketListener a] -> m ()
mergeListeners _ [] = return ()
mergeListeners output xs = foldl1 (>>) $ fmap (runListener output) xs    
    
-- | The second argument is your reactive-banana graph, the third argument is the listeners
initializeWithReactive
  :: MonadIO m =>
     ServerAPI m
     -> (Event (SocketInput a) -> MomentIO ())
     -> [SocketListener a]
     -> IO (m ())
initializeWithReactive serverApi reactiveNetwork xs = do 
    (output, input) <- liftIO $ spawn unbounded
    (addHandler, fire) <- registerCallback
    liftIO $ do 
        void $ forkIO(runEffect $ fromInput input >-> fireCallback fire)
        network <- compile $ fromAddHandler addHandler >>= reactiveNetwork
        actuate network
    initialize serverApi $ mergeListeners output xs
