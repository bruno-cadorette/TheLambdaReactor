{-# LANGUAGE OverloadedStrings #-}
module WorldEngine() where
import World (Player(..),Vector2d(..),Ennemy(..),World(..))
import Bullet (Bullet(..),moveBullet)
import import qualified Data.Map.Strict as Map
import qualified Network.SocketIO as SocketIO
import qualified Control.Concurrent.STM as STM

data WorldEngine = WorldEngine Map Map Map

getNewWorld = (WorldEngine Map.empty Map.empty Map.empty);

addPlayer (WorldEngine players bullet ennemy) (Player uuid hp position orientation) =
  (WorldEngine (Map.insert (uuid,(Player uuid hp position orientation) players)) bullet ennemy)


removePlayer (WorldEngine players bullet ennemy) (Player uuid hp position orientation) =
  (WorldEngine (Map.delete uuid players) bullet ennemy)

removeHpToPlayer (WorldEngine players bullet ennemy) uuid dmg =
  (WorldEngine (Map.adjust (\ (Player uuid hp position orientation) -> (Player uuid (hp-dmg) position orientation)) uuid players) bullet ennemy)

movePlayer (WorldEngine players bullet ennemy) uuid position =
    (WorldEngine (Map.adjust (\ (Player uuid hp x orientation) -> (Player uuid hp position orientation)) uuid players) bullet ennemy)
