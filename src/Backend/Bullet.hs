{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Bullet (Bullet(..),moveBullet,expiredBullet) where
import Game.Helper
import GHC.Generics
import Linear.Vector
import Data.Time.Clock


data Bullet = Bullet {uuid :: Int, location :: Location ,velocity :: Float, playerId::Id} deriving (Generic,Show,Eq)

range :: Int
range = 1000


moveBullet :: Bullet -> Bullet
moveBullet (Bullet uuid location velocity playerId) = (Bullet uuid (moveLocation location  ((orientation location) ^* velocity)) velocity playerId)

expiredBullet :: Bullet -> UTCTime -> Bool
expiredBullet (Bullet uuid _ _ _) time = let currentTime = getCurrentMilli time
                                                          in (currentTime - uuid) >  range
