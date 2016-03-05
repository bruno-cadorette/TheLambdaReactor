{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GameState (GameState(..), Hit(..),emptyGameState) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import GHC.Generics
import Data.Map.Strict as Map
import Bullet (Bullet(..))
import Character (Entity(..))

data Hit = Hit {uuid :: Int, player :: Entity, bullet :: Bullet} deriving (Generic,Show, Eq)
data GameState = GameState {players :: Map Text.Text Entity, projectiles :: [Bullet],enemies :: [Entity], hits :: [Hit]} deriving (Generic,Show,Eq)

emptyGameState :: GameState
emptyGameState = GameState Map.empty [] [] []

--JSON stuff
