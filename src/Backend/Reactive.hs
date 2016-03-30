{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

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
    test1, test2, initializeWithReactive, initWithReactive, 
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
import Control.Concurrent.STM
import Data.Text (Text)
import Network.SocketIO
import Network.EngineIO(ServerAPI)
import Debug.Trace
import Data.Bifunctor

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


createCallback
  :: (MonadIO m, MonadState RoutingTable m, FromJSON a) =>
     Text -> m (AddHandler (SocketInput a))
createCallback text = createCallbackInner (on text . handler)

type SocketInput a = (Socket, a)
    
    
handler :: ((SocketInput a) -> IO ()) -> a -> ReaderT Socket IO ()
handler f x = ReaderT (\r -> f (r, x))    

test1 :: (MonadIO m, MonadState RoutingTable m) => Text -> m ()
test1 text = do
    (output, input, _) <- trace "spawn!" $ liftIO $ spawn' unbounded
    trace "on!" $ on text $ handler (\x -> runEffect $ producer x >-> (trace "toOutput!" $ Pipes.Concurrent.toOutput output))
    liftIO $ runEffect $ trace "fromInput!" $ fromInput input >-> consumer

    

    
consumer :: Consumer (SocketInput String) IO ()
consumer = forever $ do
    (_, str) <- trace "await!" $ await
    lift $ putStrLn str
    
    
eventnetwork
  :: (MonadMoment m) =>
     Event (Socket, String) -> m (Event Socket, Behavior Int)
eventnetwork e = mapAccum 0 $ (\ev acc -> (fst ev, acc + 1)) <$> e
    
f2 :: Event Socket -> Behavior Int -> Event (Socket, Int)
f2 ev n = fmap (\n' s -> (s, trace (show n') $ n')) n <@> ev 

f3 :: (Socket, Int) -> ReaderT Socket IO ()
f3 (s,n) = broadcastAll "numberConnected" n

fo :: Event (Socket, Int) -> Event (IO ())
fo a = Reactive.toOutput f3 <$> a
   

--f4 :: MonadMoment m => Event (Socket, Connection) -> m (Event (Socket, Int))
testEventNetwork e = do 
    (ev', n) <- eventnetwork e
    reactimate $ fo $ f2 ev' n 

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
    
    
    
allEvents :: MonadState RoutingTable m => [m (MomentIO (Event (SocketInput a)))] -> m (MomentIO(Event [SocketInput a]))
allEvents = go toEventofList--fmap (foldl (unionWith (++)) never . fmap (fmap pure) . sequenceA). sequenceA
    where
        go f = fmap (fmap f . sequenceA) . sequenceA
        
toEventofList = foldl (unionWith (++)) never . fmap (fmap pure)


testConnectionneApi :: (MonadIO m, MonadState RoutingTable m) => m (MomentIO (Event (SocketInput String)))
testConnectionneApi = do
    userConnectedSocket <- trace "allo" $ createSocketEvent "test1"
    return $ do
        userConnected  <-  userConnectedSocket
        return $ (\x -> trace (snd x) $ x) <$> userConnected
        
        
--testConnectionneApi2 :: (MonadIO m, MonadState RoutingTable m) => m ()
testConnectionneApi2 output = do
    userConnectedSocket <- createSocketEvent "test"
    liftIO $ do
        network <- compile $ do
            userConnected  <-  userConnectedSocket
            reactimate $ (\x -> runEffect $  Pipes.yield x >-> Pipes.Concurrent.toOutput output) <$> userConnected
        actuate network
            
testConnectionneApi3 output = do
    on "test" (handler (\a -> void $ atomically $ send output a))
    
testConnectionneApi4 = 
    [onListen "test" (id :: String -> String)]
       
  
onListen :: FromJSON a => Text -> (a -> b) -> SocketListener b
onListen = SocketListener
data SocketListener b where
    SocketListener :: FromJSON a => Text -> (a -> b) -> SocketListener b
    
runListener :: MonadState RoutingTable m => Output (Socket, t) -> SocketListener t -> m ()
runListener output (SocketListener text f) =
    on text (handler (void . atomically . send output . second f))
    
mergeListeners
  :: (MonadState RoutingTable m) =>
     Output (Socket, a) -> [(SocketListener a)] -> m ()
mergeListeners ouput [] = return ()
mergeListeners ouput xs = foldl1 (>>) $ fmap (runListener ouput) xs

{-data TestApi = TestInt Int | TestString String | TestList [Int]
apiExample = do
    on' "a" TestInt
    on' "b" TestString
    on' "c" TestList-}
    
--listenOn :: (FromJSON a) => Text -> (a -> b) -> SocketListener b
--listenOn text = SocketListener 
    
producer :: SocketInput String -> Producer (SocketInput String) IO ()
producer x = do
    lift $ putStrLn ("yielding " ++ snd x)
    Pipes.yield x
    
socketIOPartPipes :: Output (SocketInput a) -> Event (SocketInput a) -> IO ()
socketIOPartPipes output event = 
    trace "pipe" $ (compile $ reactimate outEvent) >>= actuate
    where 
        outEvent = fmap (\x -> runEffect $ trace "yielding" $ Pipes.yield x >-> Pipes.Concurrent.toOutput output) event
    
consumerReactive :: Consumer (SocketInput String) IO ()
consumerReactive = forever $ trace "consumerReactive" $ do
    e <- trace "await" $ await
    liftIO $ putStrLn (snd e)

consumerReactive' :: Handler (SocketInput a) -> Consumer (SocketInput a) IO ()
consumerReactive' fire = forever $ await >>= liftIO . fire
 
initWithReactive serverApi = do
    initializeWithReactive serverApi testEventNetwork testConnectionneApi4
    
initializeWithReactive
  :: (MonadIO m, FromJSON a) =>
     ServerAPI m
     -> (Event (Socket, a) -> MomentIO ())
     -> [SocketListener a]
     -> IO (m ())
initializeWithReactive
 serverApi reactiveNetwork xs = do 
    (output, input) <- liftIO $ spawn unbounded
    (addHandler, fire) <- registerCallback
    liftIO $ do 
        forkFinally (runEffect $ fromInput input >-> consumerReactive' fire) (\a -> putStrLn "input thread is dead")
        network <- compile $ fromAddHandler addHandler >>= reactiveNetwork
        actuate network
    initialize serverApi $ mergeListeners output xs

{-FPS method-}
fps:: Int -> MomentIO (Event UTCTime)
fps frame = do
    (eTime, fireTime) <- newEvent
    liftIO . forkIO . forever $
            threadDelay frame >> getCurrentTime >>= fireTime
    return eTime

    
{- This function should be used just before reactimate to map your output. -}
toOutput :: GetSocket s => (s -> ReaderT Socket m ()) -> s -> m ()
toOutput event a = runReaderT (event a) $ getSocket a

toOutputMaybe :: (GetSocket s, Monad m) => (s -> ReaderT Socket m ()) -> Maybe s -> m ()
toOutputMaybe event (Just a)  = Reactive.toOutput event a
toOutputMaybe _ Nothing  = return ()

foldp :: MonadMoment m => (a -> b -> b) -> b -> Event a -> m (Event b)
foldp f z e = accumE z (fmap f e)
