{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Bullet (Bullet(..),moveBullet) where
import Game.Helper
import qualified Data.Aeson as Aeson
import GHC.Generics
import Linear.V2
import Linear.Vector


data Bullet = Bullet {uuid :: Int, position :: V2 Float, orientation :: V2 Float ,velocity :: Float, timeStamp::Int} deriving (Generic,Show,Eq)

moveBullet (Bullet uuid position orientation velocity timestamp) = (Bullet uuid (position + (orientation ^* velocity)) orientation velocity timestamp)

--Bullet
instance Aeson.ToJSON Bullet

instance Aeson.FromJSON Bullet
