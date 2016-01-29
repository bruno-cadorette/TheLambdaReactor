{-# LANGUAGE OverloadedStrings #-}
module WorldEngine(getWorldForJSON, getNewWorld, addPlayer,removePlayer,damageToPlayer,movePlayer, WorldEngine (..)) where
import World (World(..))
import Character (Player(..),Ennemy(..), Character (..))
import Bullet (Bullet(..),moveBullet)
import qualified Data.Map.Strict as Map
import qualified Network.SocketIO as SocketIO
import qualified Control.Concurrent.STM as STM

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

    --TODO CRUD Bullet and Ennemy
