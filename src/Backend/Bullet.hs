{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Bullet (Bullet(..),moveBullet) where
import Game.Helper
import qualified Data.Aeson as Aeson
import GHC.Generics
import Linear.V2
import Linear.Vector


data Bullet = Bullet {uuid :: Int, location :: Location ,velocity :: Float, timeStamp::Int} deriving (Generic,Show,Eq)

moveBullet (Bullet uuid location velocity timestamp) = (Bullet uuid (moveLocation location  ((orientation location) ^* velocity)) velocity timestamp)
