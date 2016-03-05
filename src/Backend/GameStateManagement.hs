{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module GameStateManagement (gameStateManager,gameStateSender, UserInput,fpsClock,testManager,updateStuff) where

import Reactive
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Network.SocketIO
import Data.ByteString.Char8
import Linear.V2
import Data.Text.Encoding
import Character
import GameEngine
import GameState
import Data.Time
import UserManagement (PlayerNames)
import Game.Helper
import Lib
import Debug.Trace
type Move = V2 Float
type Direction = V2 Float
type UserName = String



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
                        fpsEvent <- fps (1000 * 1000)
                        return (fpsEvent, getSocketBehavior playerNames)

gameStateManager :: (MonadIO m, MonadState RoutingTable m) => m (MomentIO (Behavior GameEngine))
gameStateManager  = do

    return $ do
        fpsEventMoment <- fps (1000 * 1000)
        let eUpdate = (updateWorld) <$ fpsEventMoment
        accumB getNewGameState eUpdate

updateWorld :: GameEngine -> GameEngine
updateWorld m = updateBullets m

testManager :: (MonadIO m, MonadState RoutingTable m) => m (MomentIO (Event UserInput))
testManager = do
    userInputSocket <- createSocketEvent "userInput"
    userShootSocket <- createSocketEvent "userShoot"
    return $ do
      userInput  <-  userInputSocket
      userShoot <- userShootSocket
      let inputEvent = (\(s, n) -> Movement n s) <$> userInput
      let shootEvent = (\(s, n) -> Shoot n s) <$> userShoot
      return (unionWith (\ (Movement n s) (Shoot n' s') -> Both n s n' s') inputEvent shootEvent)

updateStuff :: Map.Map Socket Entity -> GameEngine -> UserInput ->  GameState
updateStuff players game input = getGameStateForJSON $handleInput players game input
            where
              handleInput players game (Movement m s) = (game, Map.update (\ x -> Just $ move x m) s players)
              handleInput players game (Shoot d s) = (handleShoot d (Map.lookup s players) game, players)
              handleInput players game (Both m s d s') = (handleShoot d (Map.lookup s players) game,  Map.update (\ x -> Just $ move x m) s players)




--Dont really need it right now
notifyMove :: GameState -> UTCTime -> EventHandler()
notifyMove n time = broadcastAll "updateGameState" (trace "sending updates!" n)

gameStateSender :: GetSocket a => GameState -> UTCTime -> a -> EventHandler()
gameStateSender game time sock = notifyMove game time

gameStateSenderTest x _ = broadcast "updateGameState" (getGameStateForJSON x)
