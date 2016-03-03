{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Character (Entity (..), Character (..), Id ) where

import Game.Helper
import Linear.V2
import Linear.Vector  ((^+^))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import GHC.Generics

type Id = Text.Text

class Character a where
  hurt :: a -> Int -> a
  move :: a -> V2 Float -> a -- Move is the deplacement vector
  isDead :: a -> Bool


data Entity = Player {uuid :: Id, hp :: Int, position :: V2 Float, orientation :: V2 Float} | Enemy {uuid :: Id, hp :: Int, position :: V2 Float, orientation :: V2 Float} deriving (Generic,Show, Eq)

instance Character Entity where
  hurt p dmg = p {hp = ((hp p)  - dmg)}
  move p pos = p {position = (position p) ^+^ pos , orientation = (normalize pos) :: V2 Float }
  isDead (Player _ health _ _) = health <= 0
  isDead (Enemy _ health _ _) = health <= 0

instance Aeson.ToJSON Entity where
  toJSON p = Aeson.object [ "uuid"  Aeson..= uuid p , "hp"  Aeson..= hp p, "position"  Aeson..= position p ]
instance Aeson.FromJSON Entity
