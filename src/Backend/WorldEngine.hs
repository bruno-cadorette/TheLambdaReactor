{-# LANGUAGE OverloadedStrings #-}
module WorldEngine(getWorldForJSON, getNewWorld, addPlayer,removePlayer,damageToPlayer,movePlayer, WorldEngine (..),getPlayer,handleControl) where
import World (World(..))
import Character (Player(..),Ennemy(..), Character (..))
import Bullet (Bullet(..),moveBullet)
import qualified Data.Map.Strict as Map
import qualified Network.SocketIO as SocketIO
import qualified Control.Concurrent.STM as STM
import Linear.V2

data WorldEngine = WorldEngine (Map.Map Int Player) (Map.Map Int Bullet) (Map.Map Int Ennemy) deriving (Eq)

getWorldForJSON :: WorldEngine -> World
getWorldForJSON (WorldEngine players bullet ennemy) = (World [y | (_,y) <- Map.toList players] [y | (_,y) <- Map.toList bullet] [y | (_,y) <- Map.toList ennemy] [])

getNewWorld = (WorldEngine Map.empty Map.empty Map.empty);

addPlayer (WorldEngine players bullet ennemy) (Player uuid hp position orientation) =
  (WorldEngine (Map.insert uuid (Player uuid hp position orientation) players) bullet ennemy)


removePlayer (WorldEngine players bullet ennemy) (Player uuid hp position orientation) =
  (WorldEngine (Map.delete uuid players) bullet ennemy)

damageToPlayer (WorldEngine players bullet ennemy) uuid dmg =
  (WorldEngine (Map.adjust (\ p -> hurt p dmg) uuid players) bullet ennemy)

movePlayer (WorldEngine players bullet ennemy) uuid position =
    (WorldEngine (Map.adjust (\ p -> move p position) uuid players) bullet ennemy)


--Should not use this too often, just a helper function
getPlayer :: WorldEngine -> Int -> Maybe(Player)
getPlayer (WorldEngine players bullet ennemy) uuid = Map.lookup uuid players

    --TODO CRUD Bullet and Ennemy
    --TODO get Hit?

mouvement :: String -> V2 Float
mouvement x = case x of
               "w"  -> V2   0.0    1.0
               "a"  -> V2 (-1.0)   0.0
               "s"  -> V2   0.0  (-1.0)
               "d"  -> V2   1.0    0.0
               "wa" -> V2 (-1.0)   1.0
               "wd" -> V2   1.0    1.0
               "sa" -> V2 (-1.0) (-1.0)
               "sd" -> V2   1.0  (-1.0)
               otherwise -> V2 0.0 0.0

handleControl :: WorldEngine -> String -> Int -> WorldEngine
handleControl world x uuid = movePlayer world uuid (mouvement x)
