{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Bullet (Bullet(..),moveBullet,expiredBullet) where
import Game.Helper
import qualified Data.Aeson as Aeson
import GHC.Generics
import Linear.V2
import Linear.Vector
import Debug.Trace


data Bullet = Bullet {uuid :: Int, location :: Location ,velocity :: Float, playerId::Id} deriving (Generic,Show,Eq)
range = 1000


moveBullet (Bullet uuid location velocity playerId) = (Bullet uuid (moveLocation location  ((orientation location) ^* velocity)) velocity playerId)

expiredBullet :: Bullet -> Bool
expiredBullet (Bullet uuid location velocity playerId) = let currentTime = getCurrentMilli True
                                                          in (currentTime - uuid) >  range
