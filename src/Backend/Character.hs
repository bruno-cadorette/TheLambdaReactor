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


data Entity = Entity {hp :: Int, location :: Location} deriving (Generic,Show, Eq)

instance Character Entity where
  hurt p dmg = p {hp = ((hp p)  - dmg)}
  move p pos = p {location = changeOri (moveLocation (location p) pos) (normalize pos) }
  isDead (Entity health _) = health <= 0
