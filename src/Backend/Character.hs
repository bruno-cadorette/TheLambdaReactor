{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Character (Entity (..), Character (..),moveEntity,moveEntityBackward ) where

import Game.Helper
import Linear.V2
import Linear.Vector
import GHC.Generics

class Character a where
  hurt :: a -> Int -> a
  move :: a -> V2 Float -> a -- Move is the deplacement vector
  isDead :: a -> Bool

--Base entity
data Entity = Entity {hp :: Int, location :: Location} deriving (Generic,Show, Eq)

--Constant of the speed of a character
speed :: Float
speed = 2

moveEntity :: Entity -> Entity
moveEntity p
  | hp p >= 0 = move p $ (orientation $ location p) ^* speed
  | otherwise = p

moveEntityBackward :: Entity -> Entity
moveEntityBackward p = move p $ (orientation $ location p) ^* ((negate 1) * speed)

instance Character Entity where
  hurt p dmg = p {hp = ((hp p)  - dmg)}
  move p pos = p {location = changeOri (moveLocation (location p) pos) (normalize pos) }
  isDead (Entity health _) = health <= 0
