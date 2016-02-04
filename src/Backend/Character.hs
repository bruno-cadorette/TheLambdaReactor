{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Character (Player(..),Enemy(..), Character (..)) where

import Linear.V2
import Linear.Vector  ((^+^),unit, (*^))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics
import Control.Lens

class Character a where
  hp ::      a -> Int
  position :: a -> V2 Float
  orientation :: a -> V2 Float
  uuid :: a -> Text.Text
  hurt :: a -> Int -> a
  move :: a -> V2 Float -> a -- Move is the deplacement vector
  isDead :: a -> Bool

normalize v = (1/ magnitude) *^ v
              where magnitude = sqrt ((v ^._x) ** 2 + (v ^._y) ** 2)


data Player = Player {puuid :: Text.Text, php :: Int, pposition :: V2 Float, porientation :: V2 Float} deriving (Generic,Show, Eq)

instance Character Player where
  hp (Player _ health _ _) = health
  position (Player _ _ pos _) = pos
  orientation (Player _ _ _ orientation) = orientation
  uuid (Player uid _ _ _) = uid
  hurt p dmg = p {php = ((hp p) - dmg)}
  move p pos = p {pposition = (position p) ^+^ pos , porientation = (normalize pos) :: V2 Float }
  isDead (Player _ hp _ _) = hp <= 0


data Enemy = Enemy {euuid :: Text.Text, ehp :: Int, eposition :: V2 Float, eorientation :: V2 Float} deriving (Generic,Show, Eq)
instance Character Enemy where
  hp (Enemy _ health _ _) = health
  position (Enemy _ _ pos _) = pos
  orientation (Enemy _ _ _ orientation) = orientation
  uuid (Enemy uid _ _ _) = uid
  hurt p dmg = p {ehp = ((hp p)  - dmg)}
  move p pos = p {eposition = (position p) ^+^ pos , eorientation = (normalize pos) :: V2 Float }
  isDead (Enemy _ hp _ _) = hp <= 0
