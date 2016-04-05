{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Chat (Chat.initWithReactive) where

import qualified Data.Map.Strict as Map
import UserManagement
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
    transform (s,MovementIn n) = createMovement s (trace "MOVEMENT" n)
    transform (s,ShootIn n) = createShoot s n
    transform (_, _) = None

isConnection :: ApiExample -> Bool
isConnection (Connection _) = True
isConnection Disconnection = True
isConnection _ = False

isChat :: ApiExample -> Bool
isChat (ChatIn _) = True
isChat _ = False

--Take a behavior and make it an Event
bToE :: Behavior a -> Event b-> Event a
bToE b e = (\ x _ -> x) <$> b <@> e

serverEventNetwork :: Map.Map (Int, Int) Int -> Event (Socket, ApiExample)-> MomentIO ()
serverEventNetwork gameMap e = let mapBound = createMap $ Map.foldrWithKey (\ k x acc -> if x == 1 then k : acc else acc ) [] gameMap
                 in do
    let connectionEvent = filterE (\ (_,x) -> isConnection x) e
    let chatEvent = filterE (\(_,x) -> isChat x) e
    connectedPlayers <- accumB Map.empty $ (\ e' cp -> handleConnection (trace (show $snd e') $e') cp) <$> e
    movementInput <- accumB getNewGameState $ (\ _ ge -> ge) <$> e --Useless?
    (fpsEvent,sockBehavior) <- fpsClock connectedPlayers
    let sockE = filterJust $ bToE sockBehavior fpsEvent
    x <- setGameEvent2 movementInput connectedPlayers e fpsEvent mapBound
    reactimateSocket (\n' -> connectionMessageSender (mapToExport gameMap)  n') connectionEvent
    reactimateSocket (\ (s,ChatIn x') -> broadcastAll "receiveMessage" (Message (getSocketId s) x')) chatEvent
    reactimateSocket (\(_,n')-> broadcastAll "updateGameState" n') $ (\ x' sock -> (sock,x')) <$> x <@> sockE

listenerExample :: [SocketListener ApiExample]
listenerExample =
      [OnListen "newUser" Connection,
       OnDisconnect Disconnection,
       OnListen "userInput" MovementIn,
       OnListen "userShoot" ShootIn,
       OnListen "sendMessage" ChatIn]

initWithReactive :: MonadIO m => Network.EngineIO.ServerAPI m -> Map.Map (Int, Int) Int -> IO (m ())
initWithReactive serverApi mapBound = do
      initializeWithReactive serverApi (serverEventNetwork mapBound) listenerExample
