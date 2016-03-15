{-# LANGUAGE OverloadedStrings #-}
module GameEngine(getGameStateForJSON, getNewGameState, addBullet, GameEngine (..),getPlayer,handleShoot,getPlayersHit,updateBullets) where
import GameState (GameState(..))
import Character as C
import Bullet (Bullet(..),moveBullet)
import qualified Data.Map.Strict as Map
import Data.Text as T
import Linear.V2
import System.Random
import Data.Maybe
import Game.BoundingSphere
import Game.Helper as H
import Network.SocketIO
import Data.Time.Clock

randomGen:: StdGen
randomGen = mkStdGen 50

data GameEngine = GameEngine (Map.Map Text Entity) (Map.Map Text Bullet) (Map.Map Text Entity) StdGen

getGameStateForJSON :: (GameEngine, Map.Map Socket Entity) -> GameState
getGameStateForJSON ((GameEngine players bullet enemy _), mapPlayers) = (GameState (Map.mapKeys (\ s -> getSocketId s) mapPlayers) bullet (Map.elems enemy) [])

getNewGameState :: GameEngine
getNewGameState = (GameEngine Map.empty Map.empty Map.empty randomGen);

{-
addPlayer:: GameEngine -> Entity -> Text -> GameEngine
addPlayer (GameEngine players bullet enemy) (Entity hp location) uuid =
  (GameEngine (Map.insert uuid (Entity hp location) players) bullet enemy)

removePlayerWithUUID:: GameEngine -> Id -> GameEngine
removePlayerWithUUID (GameEngine players bullet enemy) uuid =
  (GameEngine (Map.delete uuid players) bullet enemy)

damageToPlayer :: GameEngine -> Id -> Int -> GameEngine
damageToPlayer (GameEngine players bullet enemy) uuid dmg =
  (GameEngine (Map.adjust (\ p -> hurt p dmg) uuid players) bullet enemy)

movePlayer :: GameEngine -> Id -> V2 Float -> GameEngine
movePlayer (GameEngine players bullet enemy) uuid position =
    (GameEngine (Map.adjust (\ p -> move p position) uuid players) bullet enemy)
-}
addBullet :: GameEngine -> Location -> Int -> Id -> GameEngine
addBullet (GameEngine players bullet enemy ran) location uuid sockId =
    (GameEngine players (Map.insert (T.pack $ show uuid) (Bullet uuid location 1.0 sockId) bullet) enemy ran)

updateBullets :: GameEngine -> GameEngine
updateBullets (GameEngine players bullets enemies ran) = GameEngine players  (Map.map moveBullet bullets) enemies ran


getPlayersHit :: GameEngine -> Map.Map a Entity -> [Entity]
getPlayersHit (GameEngine _ bullet _ _) players =
  let bulletBounding = fmap (\ x -> (BoundingSphere (H.position $ Bullet.location x) 0.5) ) $ Map.elems bullet
  in
   Prelude.filter (\x -> intersectingMany (BoundingSphere (H.position $ C.location x) 1.0) bulletBounding) $ Map.elems players

--Should not use this too often, just a helper function
getPlayer :: GameEngine -> Text -> Maybe(Entity)
getPlayer (GameEngine players _ _ _) uuid = Map.lookup uuid players

    --TODO CRUD Bullet and Enem

handleShoot :: V2 Float -> Maybe Entity -> GameEngine -> Id -> UTCTime -> GameEngine
handleShoot direction entity (GameEngine p b e gen) sockId time  =
  addBullet (GameEngine p b e gen) ((C.location $ fromJust entity) {orientation = direction})  (getCurrentMilli time) sockId
