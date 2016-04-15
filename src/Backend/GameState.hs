{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GameState (GameState(..), Hit(..),emptyGameState,mergeGameState,moveGameState) where

import qualified Data.Text as Text

import GHC.Generics
import Data.Map.Strict as Map
import Character as C
import Game.Helper as H
import Game.Map
import Bullet
import Data.Maybe
import Data.Time.Clock
import Game.BoundingSphere
import Linear.V2

data Hit = Hit {uuid :: Int, player :: Entity, bullet :: Bullet} deriving (Generic,Show, Eq)
data GameState = GameState {players :: Map Text.Text Entity, projectiles :: Map Text.Text Bullet,enemies :: [Entity], hits :: [V2 Float]} deriving (Generic,Show,Eq)

tileSize :: Float
tileSize = 32

emptyGameState :: GameState
emptyGameState = GameState Map.empty Map.empty [] []


-- A U B
--B = (A N B U B)
--B = old
--A = up
-- update old
mergeGameState :: GameState -> GameState -> GameState
mergeGameState (GameState p b e _ ) (GameState p' b' _ _ ) = let newBullet = Map.map (\ b'' -> b'' {Bullet.location = moveLocation (Bullet.location b'')  (H.position $ C.location $ fromJust $ Map.lookup (playerId b'') p') }) b
  in
   (GameState (Map.unionWith  (\ p1 p2 -> p2 {C.location = changeOri  (C.location p2) (orientation $ C.location p1)} ) p (Map.intersection p' p))
              (Map.unionWith (\ b1 _ -> b1) newBullet b') e [])

hurtPlayer :: GameState -> Id -> GameState
hurtPlayer (GameState pl pro enn hit') uuid' = (GameState (Map.update (\ p -> Just p {hp = (hp p) - 10}) uuid' pl ) pro enn (createHit (Map.lookup uuid' pl) hit'))

createHit :: Maybe Entity -> [V2 Float] -> [V2 Float]
createHit (Just entity) hit' = (H.position $ C.location $ entity):hit'
createHit Nothing hit' = hit'

moveGameState :: KdTree Point2d -> UTCTime -> GameState -> GameState
moveGameState bound time gs = hurtPlayers $ (moveAllBullet bound time (moveAllPlayer bound gs))

hurtPlayers :: GameState -> GameState
hurtPlayers gs =   let bulletBounding = fmap (\ x ->(x, (BoundingSphere (H.position $ Bullet.location x) 15) )) $ Map.elems $projectiles gs
                       hurtPlayers' = Map.foldrWithKey (\k x acc -> let hits' = Prelude.filter (\ (Bullet _ _ _ id') -> not $ id' == k) $ intersectingMany (BoundingSphere (H.position $ C.location x) 1.0) bulletBounding
                                                                      in case hits' of
                                                                         [] -> acc
                                                                         _ -> k:acc) [] $ players gs
  in
    Prelude.foldr (\ x gameState -> hurtPlayer gameState x ) gs hurtPlayers'

moveAllPlayer :: KdTree Point2d -> GameState -> GameState
moveAllPlayer bound (GameState p b e h) = (GameState (Map.map (\ p' -> if (playerCanMove p' bound) then moveEntity p' else moveEntityBackward p') p) b e h)

moveAllBullet :: KdTree Point2d -> UTCTime -> GameState -> GameState
moveAllBullet bound time (GameState p b e h) = (GameState p (Map.foldrWithKey (\ uuid' b' acc -> if bulletCanMove b' bound time then (Map.insert uuid' (moveBullet b' time) acc) else acc ) Map.empty b) e h)

bulletCanMove :: Bullet -> KdTree Point2d -> UTCTime -> Bool
bulletCanMove b gameBound time = let probBullet = divide tileSize $ H.position $ Bullet.location $ moveBullet b time in
                              case findNearestWall probBullet gameBound  of
                                                    Just p -> if(intersectBoxPos p probBullet 1.0 1.0) then
                                                                  False else
                                                                  (not $ expiredBullet b time)
                                                    Nothing -> (not $ expiredBullet b time)

playerCanMove :: Entity -> KdTree Point2d -> Bool
playerCanMove newEnt gameBound = let probEntity = divide tileSize $ H.position $ C.location $ moveEntity newEnt in
                              case findNearestWall probEntity gameBound  of
                                                    Just p -> if(intersectBoxPos p probEntity 1.0 1.0) then
                                                                  False else
                                                                  True
                                                    Nothing -> True
