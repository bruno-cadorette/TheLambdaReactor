{-# LANGUAGE OverloadedStrings #-}
module WorldEngine(getWorldForJSON, getNewWorld, addBullet, addPlayer,removePlayer,damageToPlayer,movePlayer, WorldEngine (..),getPlayer,handleControl,handleControlV2,handleShoot,getPlayersHit) where
import World (World(..))
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

data WorldEngine = WorldEngine (Map.Map Text Player) (Map.Map Int Bullet) (Map.Map Text Enemy) deriving (Eq)

getWorldForJSON :: WorldEngine -> World
getWorldForJSON (WorldEngine players bullet enemy) = (World players (Map.elems bullet) (Map.elems enemy) [])

getNewWorld = (WorldEngine Map.empty Map.empty Map.empty);

addPlayer (WorldEngine players bullet enemy) (Player uuid hp position orientation) =
  (WorldEngine (Map.insert uuid (Player uuid hp position orientation) players) bullet enemy)


removePlayer (WorldEngine players bullet enemy) (Player uuid hp position orientation) =
  (WorldEngine (Map.delete uuid players) bullet enemy)

damageToPlayer (WorldEngine players bullet enemy) uuid dmg =
  (WorldEngine (Map.adjust (\ p -> hurt p dmg) uuid players) bullet enemy)

movePlayer (WorldEngine players bullet enemy) uuid position =
    (WorldEngine (Map.adjust (\ p -> move p position) uuid players) bullet enemy)

addBullet (WorldEngine players bullet enemy) position orientation uuid =
    (WorldEngine players (Map.insert uuid (Bullet uuid position orientation 1.0 0) bullet) enemy)

getPlayersHit :: WorldEngine -> [Player]
getPlayersHit (WorldEngine players bullet _) =
  let bulletBounding = fmap (\ x -> (BoundingSphere (Bullet.position x) 0.5) ) $ Map.elems bullet
  in
   Prelude.filter (\x -> intersectingMany (BoundingSphere (Character.position x) 1.0) bulletBounding) $ Map.elems players

--Should not use this too often, just a helper function
getPlayer :: WorldEngine -> Text -> Maybe(Player)
getPlayer (WorldEngine players bullet enemy) uuid = Map.lookup uuid players

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

handleControl :: WorldEngine -> String -> Text -> WorldEngine
handleControl world x uuid = movePlayer world uuid (movement x)

handleControlV2 :: WorldEngine -> V2 Float -> Text -> WorldEngine
handleControlV2 world x uuid = movePlayer world uuid x

handleShoot :: V2 Float -> Id -> WorldEngine -> WorldEngine
handleShoot direction socketId world =
  addBullet world (Character.position $ fromJust $getPlayer world socketId) direction (fst $ randomR (1, 1000) randomGen)
