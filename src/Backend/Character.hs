{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Character (Entity (..), Character (..),moveEntity,moveEntityBackward ) where

import Game.Helper
import Linear.V2
import Linear.Vector  ((^+^), (^*))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import GHC.Generics

class Character a where
  hurt :: a -> Int -> a
  move :: a -> V2 Float -> a -- Move is the deplacement vector
  isDead :: a -> Bool


data Entity = Entity {hp :: Int, location :: Location} deriving (Generic,Show, Eq)

speed :: Float
speed = 2

moveEntity p = move p $ (orientation $ location p) ^* speed

moveEntityBackward p = move p $ (orientation $ location p) ^* ((0-1) * speed)

instance Character Entity where
  hurt p dmg = p {hp = ((hp p)  - dmg)}
  move p pos = p {location = changeOri (moveLocation (location p) pos) (normalize pos) }
  isDead (Entity health _) = health <= 0
