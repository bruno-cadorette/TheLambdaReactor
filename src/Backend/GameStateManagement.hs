{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module GameStateManagement (gameStateManager,gameStateSender, UserInput,fpsClock,testManager,updateStuff) where

import Reactive
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Network.SocketIO
import Linear.V2
import Character
import GameEngine
import GameState
import Data.Time
import UserManagement (PlayerNames)
import Game.Helper
import Lib
import Debug.Trace
import Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS
type Move = V2 Float
type Direction = V2 Float



data UserInput = Movement Move Socket | Shoot Direction Socket | Both Move Socket Direction Socket

instance GetSocket UserInput where
    getSocket (Movement _ s) = s
    getSocket (Shoot _ s) = s
    getSocket (Both _ s _ _) = s


getSocketBehavior :: Behavior PlayerNames -> Behavior (Maybe Socket)
getSocketBehavior players = (\ ma -> let sockets = (Map.keys ma) ::[Socket]
                                      in
                                       if ((not $ Prelude.null sockets)) then (Just $ Prelude.head sockets) else Nothing) <$> players

fpsClock :: Behavior PlayerNames -> MomentIO((Event UTCTime,Behavior (Maybe Socket)))
fpsClock playerNames = do
                        fpsEvent <- fps (33 * 1000)
                        return (fpsEvent, getSocketBehavior playerNames)

gameStateManager :: (MonadIO m, MonadState RoutingTable m) => m (MomentIO (Behavior GameEngine))
gameStateManager  = do

    return $ do
        fpsEventMoment <- fps (33 * 1000)
        let eUpdate = (updateWorld) <$ fpsEventMoment
        accumB getNewGameState eUpdate

updateWorld :: GameEngine -> GameEngine
updateWorld m = m

testManager :: (MonadIO m, MonadState RoutingTable m) => m (MomentIO (Event UserInput))
testManager = do
    userInputSocket <- createSocketEvent "userInput"
    userShootSocket <- createSocketEvent "userShoot"
    return $ do
      userInput  <-  userInputSocket
      userShoot <- userShootSocket
      let inputEvent = (\(s, n) -> Movement (fromJust $A.decode $ BS.pack n) s) <$> userInput
      let shootEvent = (\(s, n) -> Shoot (fromJust $A.decode $ BS.pack n) s) <$> userShoot
      return (unionWith (\ (Movement n s) (Shoot n' s') -> Both n s n' s') inputEvent shootEvent)

updateStuff :: Map.Map Socket Entity -> UTCTime  -> GameEngine -> UserInput-> GameState
updateStuff pls time game input = getGameStateForJSON $handleInput pls game input time
            where
              handleInput pls2 game (Movement m s) _  = (game, Map.update (\ x -> Just $ move x m) s pls2)
              handleInput pls2 game (Shoot d s) t = (handleShoot d (Map.lookup s pls2) game (getSocketId s) t, pls2)
              handleInput pls2 game (Both m s d _) t = (handleShoot d (Map.lookup s pls2) game (getSocketId s) t,  Map.update (\ x -> Just $ move x m) s pls2)




--Dont really need it right now
notifyMove :: GameState -> UTCTime -> EventHandler()
notifyMove n _ = broadcastAll "updateGameState" (trace "sending updates!" n)

gameStateSender :: GetSocket a => GameState -> UTCTime -> a -> EventHandler()
gameStateSender game time _ = notifyMove  game time
