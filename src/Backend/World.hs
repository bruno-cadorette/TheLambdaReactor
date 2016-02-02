{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module World (World(..)) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics
import Bullet (Bullet(..),moveBullet)
import Character (Player(..),Enemy(..), Character (..))

data Hit = Hit {uuid :: Int, player :: Player, bullet :: Bullet} deriving (Generic,Show, Eq)
data World = World {players :: [Player], projectiles :: [Bullet],ennemies :: [Enemy], hits :: [Hit]} deriving (Generic,Show,Eq)






--JSON stuff
--Player
instance Aeson.ToJSON Player

instance Aeson.FromJSON Player where
  parseJSON  = Aeson.genericParseJSON Aeson.defaultOptions
--Enemy
instance Aeson.ToJSON Enemy

instance Aeson.FromJSON Enemy where
  parseJSON  = Aeson.genericParseJSON Aeson.defaultOptions
--World
instance Aeson.ToJSON World

instance Aeson.FromJSON World where
  parseJSON  = Aeson.genericParseJSON Aeson.defaultOptions

--Hit
instance Aeson.ToJSON Hit

instance Aeson.FromJSON Hit where
  parseJSON  = Aeson.genericParseJSON Aeson.defaultOptions
