{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GameState (GameState(..), Hit(..),emptyGameState,mergeGameState,moveGameState) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import GHC.Generics
import Data.Map.Strict as Map
import Bullet (Bullet(..))
import Character as C
import Game.Helper as H
import Game.Map
import Data.List as L
import Bullet
import Data.Maybe
import Game.BoundingSphere

data Hit = Hit {uuid :: Int, player :: Entity, bullet :: Bullet} deriving (Generic,Show, Eq)
data GameState = GameState {players :: Map Text.Text Entity, projectiles :: Map Text.Text Bullet,enemies :: [Entity], hits :: [Hit]} deriving (Generic,Show,Eq)

tileSize :: Float
tileSize = 32

emptyGameState :: GameState
emptyGameState = GameState Map.empty Map.empty [] []

-- update old
mergeGameState :: GameState -> GameState -> GameState
mergeGameState (GameState p b e _ ) (GameState p' b' _ _ ) = let newBullet = Map.map (\ b'' -> b'' {Bullet.location = moveLocation (Bullet.location b'')  (H.position $ C.location $ fromJust $ Map.lookup (playerId b'') p') }) b
  in
   (GameState (Map.unionWith  (\ p1 p2 -> p2 {C.location = changeOri  (C.location p2) (orientation $ C.location p1)} ) p p')
              (Map.unionWith (\ b1 b2 -> b1) newBullet b') e [])

moveGameState :: KdTree Point2d -> GameState -> GameState
moveGameState bound gs = (moveAllBullet bound (moveAllPlayer bound gs))

moveAllPlayer :: KdTree Point2d -> GameState -> GameState
moveAllPlayer bound (GameState p b e h) = (GameState (Map.map (\ p' -> if (playerCanMove p' bound) then moveEntity p' else moveEntityBackward p') p) b e h)

moveAllBullet :: KdTree Point2d -> GameState -> GameState
moveAllBullet bound (GameState p b e h) = (GameState p (Map.foldrWithKey (\ uuid b' acc -> if bulletCanMove b' bound then (Map.insert uuid (moveBullet b') acc) else acc ) Map.empty b) e h)

bulletCanMove :: Bullet -> KdTree Point2d -> Bool
bulletCanMove b gameBound = let probBullet = divide tileSize $ H.position $ Bullet.location $ moveBullet b in
                              case findNearestWall probBullet gameBound  of
                                                    Just p -> if(intersectBoxPos p probBullet 1.0 1.0) then
                                                                  False else
                                                                  True && (not $ expiredBullet b)
                                                    Nothing -> True && (not $ expiredBullet b)

playerCanMove :: Entity -> KdTree Point2d -> Bool
playerCanMove newEnt gameBound = let probEntity = divide tileSize $ H.position $ C.location $ moveEntity newEnt in
                              case findNearestWall probEntity gameBound  of
                                                    Just p -> if(intersectBoxPos p probEntity 1.0 1.0) then
                                                                  False else
                                                                  True
                                                    Nothing -> True
