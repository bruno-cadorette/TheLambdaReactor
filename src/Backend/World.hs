{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module World (World(..), Hit(..)) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics
import Data.Map.Strict
import Bullet (Bullet(..),moveBullet)
import Character (Player(..),Enemy(..), Character (..))

data Hit = Hit {uuid :: Int, player :: Player, bullet :: Bullet} deriving (Generic,Show, Eq)
data World = World {players :: Map Text.Text Player, projectiles :: [Bullet],ennemies :: [Enemy], hits :: [Hit]} deriving (Generic,Show,Eq)






--JSON stuff
--Player
instance Aeson.ToJSON Player where
toJSON (Player uuid hp position orientation) = Aeson.object [ "uuid"  Aeson..= uuid , "hp"  Aeson..= hp, "position"  Aeson..= position ]

instance Aeson.FromJSON Player
--Enemy
instance Aeson.ToJSON Enemy where
  toJSON (Enemy uuid hp position orientation) = Aeson.object [ "uuid"  Aeson..= uuid , "hp"  Aeson..= hp, "position"  Aeson..= position ]

instance Aeson.FromJSON Enemy
--World
instance Aeson.ToJSON World where
  toJSON (World players projectiles enemies hits) = Aeson.object ["players" Aeson..= Aeson.toJSON (toList players),
                                                                   "projectiles" Aeson..= Aeson.toJSON projectiles,
                                                                   "enemies" Aeson..= Aeson.toJSON enemies,
                                                                   "hits" Aeson..= Aeson.toJSON hits]

instance Aeson.FromJSON World

--Hit
instance Aeson.ToJSON Hit

instance Aeson.FromJSON Hit
