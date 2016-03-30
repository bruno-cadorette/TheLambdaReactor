{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Reactive {-# DEPRECATED "Use Network.SocketIO.Reactive instead" #-} (
    createSocketEvent,
    disconnectEvent,
    Reactive.toOutput,
    module Reactive.Banana,
    module Reactive.Banana.Frameworks,
    module Network.SocketIO.Reactive,
    fps,
    initWithReactive, 
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
import Data.Text (Text)
import Network.SocketIO
import Network.EngineIO(ServerAPI)
import Debug.Trace
import Network.SocketIO.Reactive


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


createCallback
  :: (MonadIO m, MonadState RoutingTable m, FromJSON a) =>
     Text -> m (AddHandler (SocketInput a))
createCallback text = createCallbackInner (on text . handler)    
    
handler :: ((SocketInput a) -> IO ()) -> a -> ReaderT Socket IO ()
handler f x = ReaderT (\r -> f (r, x))    
    
    
eventnetwork
  :: (MonadMoment m) =>
     Event (Socket, ApiExample) -> m (Event Socket, Behavior Int)
eventnetwork e = mapAccum 0 $ (\ev acc -> (fst ev, acc + 1)) <$> e
    
f2 :: Event Socket -> Behavior Int -> Event (Socket, Int)
f2 ev n = fmap (\n' s -> (s, n')) n <@> ev 
   

testEventNetwork :: Event (Socket, ApiExample) -> MomentIO ()
testEventNetwork e = do 
    (ev', n) <- eventnetwork e
    reactimateSocket (\(_,n)-> broadcastAll "numberConnected" $ trace (show n) $ n) $ f2 ev' n 

    
        
    

{- Create an Event from an 'on' EventHandler -}
createSocketEvent :: (MonadIO m, FromJSON a, MonadState RoutingTable m) =>
    Text -> m (MomentIO (Event (Socket, a)))
createSocketEvent = trace "createSocketEventCalled" . fmap fromAddHandler . createCallback

{- Create an Event from the disconnect EventHandler -}
disconnectEvent :: (MonadIO m, MonadState RoutingTable m) => m (MomentIO (Event (Socket, ())))
disconnectEvent = fmap fromAddHandler disconnectCallback

data ApiExample = Connection String | Count Int | Disconnection | Test String

listenerExample :: [SocketListener ApiExample]
listenerExample = 
    [OnListen "connection" Connection, 
     OnDisconnect Disconnection,
     OnListen "count" Count,
     OnListen "test" Test]
     
initWithReactive ::
                  MonadIO m =>
                  Network.EngineIO.ServerAPI m -> IO (m ())
                  
initWithReactive serverApi = do
    initializeWithReactive serverApi testEventNetwork listenerExample

{-FPS method-}
fps:: Int -> MomentIO (Event UTCTime)
fps frame = do
    (eTime, fireTime) <- newEvent
    void $ liftIO . forkIO . forever $
            threadDelay frame >> getCurrentTime >>= fireTime
    return eTime
    
{- This function should be used just before reactimate to map your output. -}
toOutput :: GetSocket s => (s -> ReaderT Socket m ()) -> s -> m ()
toOutput event a = runReaderT (event a) $ getSocket a

toOutputMaybe :: (GetSocket s, Monad m) => (s -> ReaderT Socket m ()) -> Maybe s -> m ()
toOutputMaybe event (Just a)  = Reactive.toOutput event a
toOutputMaybe _ Nothing  = return ()