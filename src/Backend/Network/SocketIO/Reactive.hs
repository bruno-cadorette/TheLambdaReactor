{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}


module Network.SocketIO.Reactive(
    GetSocket(..),
    SocketInput,
    broadcastAll,
    SocketListener(..),
    reactimateSocket,
    initializeWithReactive) where

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
import Network.SocketIO
import Network.EngineIO(ServerAPI)
import Data.Bifunctor


class GetSocket a where
    getSocket :: a -> Socket

instance GetSocket (Socket, a) where
    getSocket = fst


instance GetSocket Socket where
    getSocket = id


type SocketInput a = (Socket, a)

handler :: ((SocketInput a) -> IO ()) -> a -> ReaderT Socket IO ()
handler f x = ReaderT (\r -> f (r, x))

registerCallback :: (MonadIO m) => IO (AddHandler (Socket, a), (Socket, a) -> m ())
registerCallback = do
    (addHandler, fire) <- newAddHandler
    void $ register addHandler (const $ pure ())
    return (addHandler, liftIO . fire)

{- Broadcast to every socket, including the current socket -}
broadcastAll :: (MonadIO m, MonadReader Socket m, ToJSON a) => Text -> a -> m ()
broadcastAll text x = do
    emit text x
    broadcast text x

reactimateSocket :: GetSocket s => (s -> ReaderT Socket IO ()) -> Event s -> MomentIO ()
reactimateSocket f = reactimate . fmap (\a -> runReaderT (f a) $ getSocket a)


fireCallback :: Handler (SocketInput a) -> Consumer (SocketInput a) IO ()
fireCallback fire = forever $ await >>= liftIO . fire

data SocketListener b where
    OnListen :: FromJSON a => Text -> (a -> b) -> SocketListener b
    OnDisconnect ::  b -> SocketListener b

runListener :: MonadState RoutingTable m => Output (Socket, t) -> SocketListener t -> m ()
runListener output (OnListen text f) =
    on text (handler (void . atomically . send output . second f))
runListener output (OnDisconnect a) =
    appendDisconnectHandler (handler (\(s, ()) -> void $ atomically $ send output (s, a)) ())

mergeListeners
  :: (MonadState RoutingTable m) =>
     Output (Socket, a) -> [(SocketListener a)] -> m ()
mergeListeners _ [] = return ()
mergeListeners output xs = foldl1 (>>) $ fmap (runListener output) xs

initializeWithReactive
  :: (MonadIO m) =>
     ServerAPI m
     -> (Event (Socket, a) -> MomentIO ())
     -> [SocketListener a]
     -> IO (m ())
initializeWithReactive
 serverApi reactiveNetwork xs = do
    (output, input) <- liftIO $ spawn unbounded
    (addHandler, fire) <- registerCallback
    liftIO $ do
        void $ forkIO(runEffect $ fromInput input >-> fireCallback fire)
        network <- compile $ fromAddHandler addHandler >>= reactiveNetwork
        actuate network
    initialize serverApi $ mergeListeners output xs
