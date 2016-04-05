{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module GameStateManagement ( UserInput(..),fpsClock,updateStuff,createMovement,createShoot) where

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

data UserInput = Movement Move Socket | Shoot Direction Socket | Both Move Socket Direction Socket | None

instance GetSocket UserInput where
    getSocket (GameStateManagement.Movement _ s) = s
    getSocket (GameStateManagement.Shoot _ s) = s
    getSocket (Both _ s _ _) = s


getSocketBehavior :: Behavior PlayerNames -> Behavior (Maybe Socket)
getSocketBehavior players' = (\ ma -> let sockets = (Map.keys ma) ::[Socket]
                                      in
                                       if ((not $ Prelude.null sockets)) then (Just $ Prelude.head sockets) else Nothing) <$> players'

fpsClock :: Behavior PlayerNames -> MomentIO((Event UTCTime,Behavior (Maybe Socket)))
fpsClock playerNames = do
                        fpsEvent <- fps (33 * 1000)
                        return (fpsEvent, getSocketBehavior playerNames)

updateStuff :: Map.Map Socket Entity -> UTCTime  -> GameEngine -> UserInput-> GameState
updateStuff pls time game input = getGameStateForJSON $handleInput pls game input time
            where
              handleInput pls2 game' (Movement m s) _  = (game', Map.update (\ x -> Just $ move x (trace ((show m)++"UPDATESTUFF") m)) s pls2)
              handleInput pls2 game' (Shoot d s) t = (handleShoot d (Map.lookup s pls2) game' (getSocketId s) t, pls2)
              handleInput pls2 game' (Both m s d _) t = (handleShoot d (Map.lookup s pls2) game' (getSocketId s) t,  Map.update (\ x -> Just $ move x m) s pls2)
              handleInput pls2 game' None _ = (game',pls2)


--Create game record from communication
createMovement :: Socket -> V2 Float -> UserInput
createMovement s n = Movement n s

createShoot :: Socket -> V2 Float -> UserInput
createShoot s n = Shoot n s
