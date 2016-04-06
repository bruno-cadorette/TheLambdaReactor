{-# LANGUAGE OverloadedStrings #-}
module GameEngine(getGameStateForJSON, getNewGameState, addBullet, GameEngine (..),getPlayer,handleShoot) where
import GameState (GameState(..))
import Character as C
import Bullet
import qualified Data.Map.Strict as Map
import Data.Text as T
import Linear.V2
import System.Random

import Game.Helper as H
import Network.SocketIO
import Data.Time.Clock

randomGen:: StdGen
randomGen = mkStdGen 50

data GameEngine = GameEngine (Map.Map Text Entity) (Map.Map Text Bullet) (Map.Map Text Entity) StdGen

getGameStateForJSON :: (GameEngine, Map.Map Socket Entity) -> GameState
getGameStateForJSON ((GameEngine _ bullet enemy _), mapPlayers) = (GameState (Map.mapKeys (\ s -> getSocketId s) mapPlayers) bullet (Map.elems enemy) [])

getNewGameState :: GameEngine
getNewGameState = (GameEngine Map.empty Map.empty Map.empty randomGen);

addBullet :: GameEngine -> Location -> Int -> Id -> GameEngine
addBullet (GameEngine players' bullet enemy ran) location' uuid' sockId =
    (GameEngine players' (Map.insert (T.pack $ show uuid') (Bullet uuid' location' 0.2 sockId) bullet) enemy ran)


--Should not use this too often, just a helper function
getPlayer :: GameEngine -> Text -> Maybe(Entity)
getPlayer (GameEngine players' _ _ _) uuid' = Map.lookup uuid' players'


--Handle shooting event
handleShoot :: V2 Float -> Maybe Entity -> GameEngine -> Id -> UTCTime -> GameEngine
handleShoot direction (Just entity) (GameEngine p b e gen) sockId time  =
  addBullet (GameEngine p b e gen) ((C.location entity) {orientation = direction})  (getCurrentMilli time) sockId
handleShoot _ Nothing g _ _ = g
