{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Reactive (
    createSocketEvent,
    disconnectEvent,
    Reactive.toOutput,
    broadcastAll,
    foldp,
    GetSocket(..),
    module Reactive.Banana,
    module Reactive.Banana.Frameworks,
    fps,
    test1, test2,
    toOutputMaybe) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Concurrent
import Data.Time
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Aeson
import Pipes.Concurrent
import Pipes
import Data.Text
import Network.SocketIO
import Debug.Trace

class GetSocket a where
    getSocket :: a -> Socket

instance GetSocket (Socket, a) where
    getSocket = fst

instance GetSocket Socket where
    getSocket = id
    




createCallbackInner
  :: (FromJSON a, MonadIO m) =>
     (Handler (SocketInput a) -> m b) -> m (AddHandler (SocketInput a))
createCallbackInner callback = do
    (addHandler, fire) <- liftIO $ registerCallback
    void $ callback fire
    return addHandler


disconnectCallback :: (MonadIO m, MonadState RoutingTable m) => m (AddHandler (Socket, ()))
disconnectCallback = createCallbackInner (\fire -> appendDisconnectHandler $ handler fire ())

registerCallback :: (MonadIO m, FromJSON a) => IO (AddHandler (Socket, a), (Socket, a) -> m ())
registerCallback = do
    (addHandler, fire) <- newAddHandler
    void $ register addHandler (const $ pure ())
    return (addHandler, liftIO . fire)
    
--producerFromSocketEvent : (MonadIO m, FromJSON a, MonadState RoutingTable m) => Text -> Output a -> m (AddHandler (Socket, a))
--producerFromSocketEvent text output = on text . handler . atomically . 



createCallback
  :: (MonadIO m, MonadState RoutingTable m, FromJSON a) =>
     Text -> m (AddHandler (SocketInput a))
createCallback text = createCallbackInner (on text . handler)
{-
createCallback text = do
    (output, input, seal) <- liftIO $ spawn' unbounded
    on text $ trace "receiveSomething" . (handler (sendToMainThread output))
    a <- liftIO $ createCallbackInner (\fire -> getFromSocketIOThread fire input)
    liftIO $ atomically seal
    return a-}

type SocketInput a = (Socket, a)
    
    
handler :: ((SocketInput a) -> IO ()) -> a -> ReaderT Socket IO ()
handler f x = ReaderT (\r -> f (r, x))    


test1 :: (MonadIO m, MonadState RoutingTable m) => Text -> m ()
test1 text = do
    (output, input, _) <- trace "spawn!" $ liftIO $ spawn' unbounded
    trace "on!" $ on text $ handler (\x -> runEffect $ producer x >-> (trace "toOutput!" $ Pipes.Concurrent.toOutput output))
    liftIO $ runEffect $ trace "fromInput!" $ fromInput input >-> consumer

    
producer :: SocketInput String -> Producer (SocketInput String) IO ()
producer x = do
    lift $ putStrLn ("yielding " ++ snd x)
    Pipes.yield x
    
    
consumer :: Consumer (SocketInput String) IO ()
consumer = do
    (_, str) <- trace "await!" $ await
    lift $ putStrLn str
    
    
    
    
    
    
    
    
    
pipeHandler :: ((SocketInput a) -> IO ()) -> Consumer (SocketInput a) IO ()
pipeHandler f = do
    x <- trace "await!" $ await
    lift $ f x
    
    
    
    
    
            
getFromSocketIOThread :: (FromJSON a) => ((SocketInput a) -> IO ()) -> Input (SocketInput a) -> IO ()
getFromSocketIOThread f input =  
    runEffect $ trace "fromInput!" $ fromInput input >-> pipeHandler f
    
    
sendToMainThread :: (FromJSON a) => Output (SocketInput a) -> SocketInput a -> IO ()
sendToMainThread output x =  
    runEffect $ (trace "yield!" $ Pipes.yield x) >-> (trace "toOutput!" $ Pipes.Concurrent.toOutput output)
    
test2 text = on text $ handler (putStrLn . snd)
    

{- Create an Event from an 'on' EventHandler -}
createSocketEvent :: (MonadIO m, FromJSON a, MonadState RoutingTable m) =>
    Text -> m (MomentIO (Event (Socket, a)))
createSocketEvent = trace "createSocketEventCalled" . fmap fromAddHandler . createCallback

{- Create an Event from the disconnect EventHandler -}
disconnectEvent :: (MonadIO m, MonadState RoutingTable m) => m (MomentIO (Event (Socket, ())))
disconnectEvent = fmap fromAddHandler disconnectCallback

{- Broadcast to every socket, including the current socket -}
broadcastAll :: (MonadIO m, MonadReader Socket m, ToJSON a) => Text -> a -> m ()
broadcastAll text x = do
    emit text x
    broadcast text x


    


{-FPS method-}
fps:: Int -> MomentIO (Event UTCTime)
fps frame = do
    (eTime, fireTime) <- newEvent
    liftIO . forkIO . forever $
            threadDelay frame >> getCurrentTime >>= (trace "new FPS Event" fireTime)
    return eTime

{- This function should be used just before reactimate to map your output. -}
toOutput :: GetSocket s => (s -> ReaderT Socket m ()) -> s -> m ()
toOutput event a = runReaderT (event a) $ getSocket a

toOutputMaybe :: (GetSocket s, Monad m) => (s -> ReaderT Socket m ()) -> Maybe s -> m ()
toOutputMaybe event (Just a)  = Reactive.toOutput event a
toOutputMaybe _ Nothing  = return ()

foldp :: MonadMoment m => (a -> b -> b) -> b -> Event a -> m (Event b)
foldp f z e = accumE z (fmap f e)
