{-# LANGUAGE OverloadedStrings #-}
module GameEngine(getGameStateForJSON, getNewGameState, addBullet, addPlayer,removePlayer,damageToPlayer,movePlayer, GameEngine (..),getPlayer,handleControlV2,handleShoot,getPlayersHit,updateBullets) where
import GameState (GameState(..))
import Character
import Bullet (Bullet(..),moveBullet)
import qualified Data.Map.Strict as Map
import Data.Text
import Linear.V2
import System.Random
import Data.Maybe
import Game.BoundingSphere

randomGen:: StdGen
randomGen = mkStdGen 50

data GameEngine = GameEngine (Map.Map Text Entity) (Map.Map Int Bullet) (Map.Map Text Entity) deriving (Eq)

getGameStateForJSON :: GameEngine -> GameState
getGameStateForJSON (GameEngine players bullet enemy) = (GameState players (Map.elems bullet) (Map.elems enemy) [])

getNewGameState :: GameEngine
getNewGameState = (GameEngine Map.empty Map.empty Map.empty);

addPlayer:: GameEngine -> Entity -> GameEngine
addPlayer (GameEngine players bullet enemy) (Player uuid hp position orientation) =
  (GameEngine (Map.insert uuid (Player uuid hp position orientation) players) bullet enemy)

removePlayer:: GameEngine -> Entity -> GameEngine
removePlayer (GameEngine players bullet enemy) (Player uuid hp position orientation) =
  (GameEngine (Map.delete uuid players) bullet enemy)

damageToPlayer :: GameEngine -> Id -> Int -> GameEngine
damageToPlayer (GameEngine players bullet enemy) uuid dmg =
  (GameEngine (Map.adjust (\ p -> hurt p dmg) uuid players) bullet enemy)

movePlayer :: GameEngine -> Id -> V2 Float -> GameEngine
movePlayer (GameEngine players bullet enemy) uuid position =
    (GameEngine (Map.adjust (\ p -> move p position) uuid players) bullet enemy)

addBullet :: GameEngine -> V2 Float -> V2 Float -> Int -> GameEngine
addBullet (GameEngine players bullet enemy) position orientation uuid =
    (GameEngine players (Map.insert uuid (Bullet uuid position orientation 1.0 0) bullet) enemy)

updateBullets :: GameEngine -> GameEngine
updateBullets (GameEngine players bullets ennemies) = GameEngine players  (Map.map moveBullet bullets) ennemies


getPlayersHit :: GameEngine -> [Entity]
getPlayersHit (GameEngine players bullet _) =
  let bulletBounding = fmap (\ x -> (BoundingSphere (Bullet.position x) 0.5) ) $ Map.elems bullet
  in
   Prelude.filter (\x -> intersectingMany (BoundingSphere (Character.position x) 1.0) bulletBounding) $ Map.elems players

--Should not use this too often, just a helper function
getPlayer :: GameEngine -> Text -> Maybe(Entity)
getPlayer (GameEngine players bullet enemy) uuid = Map.lookup uuid players

    --TODO CRUD Bullet and Enemy


handleControlV2 :: GameEngine -> V2 Float -> Text -> GameEngine
handleControlV2 gameState x uuid = movePlayer gameState uuid x

handleShoot :: V2 Float -> Id -> GameEngine -> GameEngine
handleShoot direction socketId gameState =
  addBullet gameState (Character.position $ fromJust $getPlayer gameState socketId) direction (fst $ randomR (1, 1000) randomGen)
