{-# LANGUAGE OverloadedStrings #-}

module World (Player(..),Vector2d(..),Bullet(..),Ennemy(..),World(..)) where

data Vector2d = Vector2d {x :: Float, y :: Float} deriving (Generic,Show)
data Player = Player {uuid :: Int, hp :: Int, position :: Vector2d, orientation :: Vector2d} deriving (Generic,Show)
data Bullet = Bullet {uuid :: Int, initialPosition :: Vector2d, orientation :: Vector2d, timeStamp::Int} deriving (Generic,Show)
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
--Bullet
instance Aeson.ToJSON Bullet

instance Aeson.FromJSON Bullet where
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
