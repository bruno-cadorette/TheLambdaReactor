{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Chat (Chat.initWithReactive) where

import Reactive
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import UserManagement
import Data.Text
import Message
import Network.SocketIO
import GameState
import GameStateManagement
import Game.MapReader
import Game.Map
import GameEngine
import Character
import Data.Time.Clock
import Lib
import Network.SocketIO.Reactive
import Network.EngineIO(ServerAPI)
import Debug.Trace

type Move = V2 Float
type Direction = V2 Float

sendMessage :: PlayerNames -> (Socket, Text) -> EventHandler ()
sendMessage p (s, n) =
    case Map.lookup s p of
        Just x -> broadcastAll "receiveMessage" $ Message (pack (show x)) n
        Nothing -> broadcastAll "receiveMessage" $ Message "ERROR USER" n

setGameEvent :: MomentIO (Behavior GameEngine) -> Behavior (Map.Map Socket Entity) -> Event UserInput -> Event a -> KdTree Point2d -> MomentIO (Behavior GameState)
setGameEvent inputSocket connectedPlayers inputEvent fpsEvent mapBound = do
  bcurrentTime <- fromPoll getCurrentTime
  gameStateObject <- inputSocket
  let mix = updateStuff <$> connectedPlayers <*> bcurrentTime <*> gameStateObject <@> inputEvent
  gameObject <- accumB emptyGameState $ fmap mergeGameState mix
  gameUpdated <- accumB  emptyGameState $((\ updates currenTime _ old -> (moveGameState mapBound currenTime) $ mergeGameState updates old ) <$> gameObject <*> bcurrentTime <@> fpsEvent)
  return gameUpdated

setGameEvent2 :: (Behavior GameEngine) -> Behavior (Map.Map Socket Entity) -> Event (Socket, ApiExample) -> Event a -> KdTree Point2d -> MomentIO (Behavior GameState)
setGameEvent2 inputSocket connectedPlayers inputEvent fpsEvent mapBound = do
  let inputEventTrans = transform <$> inputEvent
  bcurrentTime <- fromPoll getCurrentTime
  --gameStateObject <- inputSocket
  let mix = updateStuff <$> connectedPlayers <*> bcurrentTime <*> inputSocket <@> inputEventTrans
  gameObject <- accumB emptyGameState $ fmap mergeGameState mix
  gameUpdated <- accumB  emptyGameState $((\ updates currenTime _ old -> (moveGameState mapBound currenTime) $ mergeGameState updates old ) <$> gameObject <*> bcurrentTime <@> fpsEvent)
  return gameUpdated
  where
    transform (s,MovementIn n) = createMovement s ((trace "MOVEMENT" $ unpack n))
    transform (s,ShootIn n) = createShoot s $ unpack n
    transform (s, _) = None

{-server :: (MonadIO m, MonadState RoutingTable m) =>  Map.Map (Int, Int) Int -> m ()
server gameMap =let mapBound = createMap $ Map.foldrWithKey (\ k x acc -> if x == 1 then k : acc else acc ) [] gameMap
                 in do
    sendMessageSocket   <- createSocketEvent "sendMessage"
    usersSocket <- connectionManager
    inputSocket <- gameStateManager
    testSocket <- testManager

    liftIO $ do
        network <- compile $ do
            sendMessageEvent <- sendMessageSocket
            inputEvent <- testSocket
            (connectionEvent, connectedPlayers) <- usersSocket
            (fpsEvent,sockBehavior) <- fpsClock connectedPlayers
            x <- setGameEvent inputSocket connectedPlayers inputEvent fpsEvent mapBound
            reactimate $ (toOutput . (connectionMessageSender $ mapToExport gameMap)) <$> connectedPlayers <@> connectionEvent
            reactimate $ (toOutput . sendMessage) <$> connectedPlayers <@> sendMessageEvent
            reactimate $ (\ g s t -> toOutputMaybe (gameStateSender g t) s) <$> x <*>  sockBehavior <@> fpsEvent

        actuate network
        -}

f2 :: Event Socket -> Behavior a -> Event (Socket, a)
f2 ev n = fmap (\n' s -> (s, n')) n <@> ev
{-
f2E :: Event Socket -> Event UTCTime -> Event (SocketInput UTCTime)
f2E ev n = (fmap (\n' s -> (s, n')) n) ev
-}
eventnetwork :: (MonadMoment m) => Event (Socket, ApiExample) -> m (Event Socket, Behavior ApiExample)
eventnetwork e = mapAccum Disconnection $ (\ev acc -> (fst ev, (trace (show $snd ev) (snd ev)))) <$> e

handleEvent :: PlayerNames -> GameEngine -> (Socket,ApiExample) -> (PlayerNames,GameEngine)
handleEvent pl ge (s,(MovementIn m)) = (pl,ge)
handleEvent pl ge (s,(ShootIn sh)) = (pl,ge)
handleEvent pl ge (s,(Disconnection)) = (pl,ge)
handleEvent pl ge (s,_) = (pl,ge)

isConnection :: ApiExample -> Bool
isConnection (Connection n) = True
isConnection (Disconnection) = True
isConnection _ = False

bToE :: Behavior a -> Event b-> Event a
bToE b e = (\ x y -> x) <$> b <@> e

testEventNetwork :: Map.Map (Int, Int) Int -> Event (Socket, ApiExample)-> MomentIO ()
testEventNetwork gameMap e = let mapBound = createMap $ Map.foldrWithKey (\ k x acc -> if x == 1 then k : acc else acc ) [] gameMap
                 in do
    (ev', n) <- eventnetwork e
    let connectionEvent = filterE (\ (s,x) -> isConnection x) e
    connectedPlayers <- accumB Map.empty $ (\ e' cp -> handleConnection (trace (show $snd e') $e') cp) <$> e
    movementInput <- accumB getNewGameState $ (\ e' ge -> ge) <$> e
    (fpsEvent,sockBehavior) <- fpsClock connectedPlayers
    let sockE = filterJust $ bToE sockBehavior fpsEvent
    x <- setGameEvent2 movementInput connectedPlayers e fpsEvent mapBound
    reactimateSocket (\n -> connectionMessageSender (mapToExport gameMap)  n) connectionEvent
    --reactimateSocket (\(_,n)-> broadcastAll "updateGameState" $ trace (show n) $ n) $ f2 ev' x
    reactimateSocket (\(s,n)-> broadcastAll "updateGameState" n) $ (\ x sock -> (sock,x)) <$> x <@> sockE

listenerExample :: [SocketListener ApiExample]
listenerExample =
      [OnListen "newUser" Connection,
       OnDisconnect Disconnection,
       OnListen "userInput" MovementIn,
       OnListen "userShoot" ShootIn]

initWithReactive :: MonadIO m => Network.EngineIO.ServerAPI m -> Map.Map (Int, Int) Int -> IO (m ())
initWithReactive serverApi mapBound = do
      initializeWithReactive serverApi (testEventNetwork mapBound) listenerExample
