{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GameState (GameState(..), Hit(..),emptyGameState,mergeGameState,moveAllPlayer) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import GHC.Generics
import Data.Map.Strict as Map
import Bullet (Bullet(..))
import Character as C
import Game.Helper as H
import Game.Map
import Game.BoundingSphere

data Hit = Hit {uuid :: Int, player :: Entity, bullet :: Bullet} deriving (Generic,Show, Eq)
data GameState = GameState {players :: Map Text.Text Entity, projectiles :: [Bullet],enemies :: [Entity], hits :: [Hit]} deriving (Generic,Show,Eq)

tileSize :: Float
tileSize = 32

emptyGameState :: GameState
emptyGameState = GameState Map.empty [] [] []

-- update old
mergeGameState :: GameState -> GameState -> GameState
mergeGameState (GameState p b e _ ) (GameState p' _ _ _ ) = (GameState (Map.unionWith  (\ p1 p2 -> p2 {C.location = changeOri  (C.location p2) (orientation $ C.location p1)} ) p p') b e [])

moveAllPlayer :: KdTree Point2d -> GameState -> GameState
moveAllPlayer bound (GameState p b e h) = (GameState (Map.map (\ p' -> if (playerCanMove p' bound) then moveEntity p' else p') p) b e h)
--JSON stuff

playerCanMove :: Entity -> KdTree Point2d -> Bool
playerCanMove newEnt gameBound = let probEntity = divide tileSize $ H.position $ C.location $ moveEntity newEnt in
                              case findNearestWall probEntity gameBound  of
                                                    Just p -> if(intersectBoxPos p probEntity 1.0 1.0) then
                                                                  False else
                                                                  True
                                                    Nothing -> True
