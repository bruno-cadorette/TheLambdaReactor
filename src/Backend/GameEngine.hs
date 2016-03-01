{-# LANGUAGE OverloadedStrings #-}
module GameEngine(getGameStateForJSON, getNewGameState, addBullet, addPlayer,removePlayer,damageToPlayer,movePlayer, GameEngine (..),getPlayer,handleControl,handleControlV2,handleShoot,getPlayersHit,updateBullets) where
import GameState (GameState(..))
import Character
import Bullet (Bullet(..),moveBullet)
import qualified Data.Map.Strict as Map
import qualified Network.SocketIO as SocketIO
import qualified Control.Concurrent.STM as STM
import Data.Text
import Network.EngineIO (SocketId) --SocketID
import Linear.V2
import System.Random
import Data.Maybe
import Game.BoundingSphere

randomGen = mkStdGen 50

data GameEngine = GameEngine (Map.Map Text Player) (Map.Map Int Bullet) (Map.Map Text Enemy) deriving (Eq)

getGameStateForJSON :: GameEngine -> GameState
getGameStateForJSON (GameEngine players bullet enemy) = (GameState players (Map.elems bullet) (Map.elems enemy) [])

getNewGameState = (GameEngine Map.empty Map.empty Map.empty);

addPlayer (GameEngine players bullet enemy) (Player uuid hp position orientation) =
  (GameEngine (Map.insert uuid (Player uuid hp position orientation) players) bullet enemy)


removePlayer (GameEngine players bullet enemy) (Player uuid hp position orientation) =
  (GameEngine (Map.delete uuid players) bullet enemy)

damageToPlayer (GameEngine players bullet enemy) uuid dmg =
  (GameEngine (Map.adjust (\ p -> hurt p dmg) uuid players) bullet enemy)

movePlayer (GameEngine players bullet enemy) uuid position =
    (GameEngine (Map.adjust (\ p -> move p position) uuid players) bullet enemy)

addBullet (GameEngine players bullet enemy) position orientation uuid =
    (GameEngine players (Map.insert uuid (Bullet uuid position orientation 1.0 0) bullet) enemy)

updateBullets :: GameEngine -> GameEngine
updateBullets (GameEngine players bullets ennemies) = GameEngine players  (Map.map moveBullet bullets) ennemies


getPlayersHit :: GameEngine -> [Player]
getPlayersHit (GameEngine players bullet _) =
  let bulletBounding = fmap (\ x -> (BoundingSphere (Bullet.position x) 0.5) ) $ Map.elems bullet
  in
   Prelude.filter (\x -> intersectingMany (BoundingSphere (Character.position x) 1.0) bulletBounding) $ Map.elems players

--Should not use this too often, just a helper function
getPlayer :: GameEngine -> Text -> Maybe(Player)
getPlayer (GameEngine players bullet enemy) uuid = Map.lookup uuid players

    --TODO CRUD Bullet and Enemy
    --TODO get Hit?

movement :: String -> V2 Float
movement x = case x of
               "w"  -> V2   0.0    1.0
               "a"  -> V2 (-1.0)   0.0
               "s"  -> V2   0.0  (-1.0)
               "d"  -> V2   1.0    0.0
               "wa" -> V2 (-1.0)   1.0
               "wd" -> V2   1.0    1.0
               "sa" -> V2 (-1.0) (-1.0)
               "sd" -> V2   1.0  (-1.0)
               otherwise -> V2 0.0 0.0

handleControl :: GameEngine -> String -> Text -> GameEngine
handleControl gameState x uuid = movePlayer gameState uuid (movement x)

handleControlV2 :: GameEngine -> V2 Float -> Text -> GameEngine
handleControlV2 gameState x uuid = movePlayer gameState uuid x

handleShoot :: V2 Float -> Id -> GameEngine -> GameEngine
handleShoot direction socketId gameState =
  addBullet gameState (Character.position $ fromJust $getPlayer gameState socketId) direction (fst $ randomR (1, 1000) randomGen)
