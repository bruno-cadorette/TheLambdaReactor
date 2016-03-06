{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GameState (GameState(..), Hit(..),emptyGameState,mergeGameState) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import GHC.Generics
import Data.Map.Strict as Map
import Bullet (Bullet(..))
import Character as C
import Game.Helper

data Hit = Hit {uuid :: Int, player :: Entity, bullet :: Bullet} deriving (Generic,Show, Eq)
data GameState = GameState {players :: Map Text.Text Entity, projectiles :: [Bullet],enemies :: [Entity], hits :: [Hit]} deriving (Generic,Show,Eq)

emptyGameState :: GameState
emptyGameState = GameState Map.empty [] [] []

mergeGameState :: GameState -> GameState -> GameState
mergeGameState (GameState p b e _ ) (GameState p' _ _ _ ) = (GameState (Map.unionWith  (\ p1 p2 -> p1 {C.location = add  (C.location p1) (C.location p2)} ) p' p) b e [])


--JSON stuff
