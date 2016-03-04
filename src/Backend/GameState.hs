{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GameState (GameState(..), Hit(..)) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import GHC.Generics
import Data.Map.Strict
import Bullet (Bullet(..))
import Character (Entity(..))

data Hit = Hit {uuid :: Int, player :: Entity, bullet :: Bullet} deriving (Generic,Show, Eq)
data GameState = GameState {players :: Map Text.Text Entity, projectiles :: [Bullet],ennemies :: [Entity], hits :: [Hit]} deriving (Generic,Show,Eq)

--JSON stuff

instance Aeson.ToJSON GameState where
  toJSON (GameState players projectiles enemies hits) = Aeson.object ["players" Aeson..= Aeson.toJSON (toList players),
                                                                   "projectiles" Aeson..= Aeson.toJSON projectiles,
                                                                   "enemies" Aeson..= Aeson.toJSON enemies,
                                                                   "hits" Aeson..= Aeson.toJSON hits]

instance Aeson.FromJSON GameState

--Hit
instance Aeson.ToJSON Hit

instance Aeson.FromJSON Hit
