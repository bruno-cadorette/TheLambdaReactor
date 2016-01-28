{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module World (Player(..),Vector2d(..),Ennemy(..),World(..)) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics
import Bullet (Bullet(..),moveBullet)

data Vector2d = Vector2d {x :: Float, y :: Float} deriving (Generic,Show)
data Player = Player {uuid :: Int, hp :: Int, position :: Vector2d, orientation :: Vector2d} deriving (Generic,Show)
data Ennemy = Ennemy {uuid :: Int, hp :: Int, position :: Vector2d, orientation :: Vector2d} deriving (Generic,Show)
data Hit = Hit {uuid :: Int, player :: Player, bullet :: Bullet} deriving (Generic,Show)
data World = World {players :: [Player], projectiles :: [Bullet],ennemies :: [Ennemy], hits :: [Hit]} deriving (Generic,Show)






--JSON stuff
--Vector2d
instance Aeson.ToJSON Vector2d

instance Aeson.FromJSON Vector2d where
  parseJSON  = Aeson.genericParseJSON Aeson.defaultOptions
--Player
instance Aeson.ToJSON Player

instance Aeson.FromJSON Player where
  parseJSON  = Aeson.genericParseJSON Aeson.defaultOptions
--Ennemy
instance Aeson.ToJSON Ennemy

instance Aeson.FromJSON Ennemy where
  parseJSON  = Aeson.genericParseJSON Aeson.defaultOptions
--World
instance Aeson.ToJSON World

instance Aeson.FromJSON World where
  parseJSON  = Aeson.genericParseJSON Aeson.defaultOptions

--Hit
instance Aeson.ToJSON Hit

instance Aeson.FromJSON Hit where
  parseJSON  = Aeson.genericParseJSON Aeson.defaultOptions
