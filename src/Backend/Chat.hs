{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Chat (Chat.initWithReactive) where

import qualified Data.Map.Strict as Map
import UserManagement
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
import Data.Text(Text)

setGameEvent2 :: Behavior GameEngine -> Behavior (Map.Map Socket Entity) -> Event (SocketInput UserInput) -> Event a -> KdTree Point2d -> MomentIO (Behavior GameState)
setGameEvent2 gameEngine connectedPlayers inputEvent fpsEvent mapBound = do
  bcurrentTime <- fromPoll getCurrentTime
  let mix = updateStuff <$> connectedPlayers <*> bcurrentTime <*> gameEngine <@> inputEvent
  gameObject <- accumB emptyGameState $ (mergeGameState mapBound) <$> bcurrentTime <@> mix
  accumB  emptyGameState $((\ updates currenTime _ old -> moveGameState mapBound currenTime $ mergeGameState mapBound currenTime updates old ) <$> gameObject <*> bcurrentTime <@> fpsEvent)

getConnectionOnly :: ApiExample -> Maybe ConnectionType
getConnectionOnly (Conn a) = Just a
getConnectionOnly _ = Nothing

getChatOnly :: ApiExample -> Maybe Text
getChatOnly (ChatIn a) = Just a
getChatOnly _ = Nothing

getInputOnly :: ApiExample -> Maybe UserInput
getInputOnly (Input a) = Just a
getInputOnly _ = Nothing

filterEventSocketInput :: (a -> Maybe b) -> Event (SocketInput a) -> Event (SocketInput b)
filterEventSocketInput f = filterJust . fmap (mapM f)


--Take a behavior and make it an Event
bToE :: Behavior a -> Event b-> Event a
bToE b e = const <$> b <@> e

serverEventNetwork :: Map.Map (Int, Int) Int -> Event (SocketInput ApiExample)-> MomentIO ()
serverEventNetwork gameMap e = let mapBound = createMap $ Map.foldrWithKey (\ k x acc -> if x == 1 then k : acc else acc ) [] gameMap
                 in do
    let connectionEvent = filterEventSocketInput getConnectionOnly e
    let chatEvent = filterEventSocketInput getChatOnly e
    let inputEvent = filterEventSocketInput getInputOnly e
    connectedPlayers <- accumB Map.empty $ handleConnection <$> connectionEvent
    movementInput <- accumB getNewGameState $ (\ _ ge -> ge) <$> e --Useless?
    (fpsEvent,sockBehavior) <- fpsClock connectedPlayers
    let sockE = filterJust $ bToE sockBehavior fpsEvent
    x <- setGameEvent2 movementInput connectedPlayers inputEvent fpsEvent mapBound
    reactimateSocket (connectionMessageSender (mapToExport gameMap)) connectionEvent
    reactimateSocket (\ (s, x') -> broadcastAll "receiveMessage" (Message (getSocketId s) x')) chatEvent
    reactimateSocket (\(_,n')-> broadcastAll "updateGameState" n') $ (\ x' sock -> (sock,x')) <$> x <@> sockE

listenerExample :: [SocketListener ApiExample]
listenerExample =
      [OnListen "newUser" (\(InitialName str) -> Conn $ Connection str),
       OnDisconnect (Conn Disconnection),
       OnListen "userInput" (Input . Movement),
       OnListen "userShoot" (Input . Shoot),
       OnListen "sendMessage" ChatIn]

initWithReactive :: MonadIO m => Network.EngineIO.ServerAPI m -> Map.Map (Int, Int) Int -> IO (m ())
initWithReactive serverApi mapBound =
      initializeWithReactive serverApi (serverEventNetwork mapBound) listenerExample
