{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module GameStateManagement (fpsClock,updateStuff) where

import Reactive
import qualified Data.Map.Strict as Map
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
import qualified Data.ByteString.Lazy.Char8 as BS


getSocketBehavior :: Behavior PlayerNames -> Behavior (Maybe Socket)
getSocketBehavior = fmap (listToMaybe . Map.keys)

fpsClock :: Behavior PlayerNames -> MomentIO((Event UTCTime,Behavior (Maybe Socket)))
fpsClock playerNames = do
                        fpsEvent <- fps (33 * 1000)
                        return (fpsEvent, getSocketBehavior playerNames)

updateStuff :: Map.Map Socket Entity -> UTCTime  -> GameEngine -> (SocketInput UserInput)-> GameState
updateStuff pls time game input = getGameStateForJSON $handleInput pls game input time
            where
              handleInput pls2 game' (s, Movement m) _  = (game', Map.update (\ x -> Just $ move x (trace ((show m)++"UPDATESTUFF") m)) s pls2)
              handleInput pls2 game' (s, Shoot d) t = (handleShoot d (Map.lookup s pls2) game' (getSocketId s) t, pls2)

