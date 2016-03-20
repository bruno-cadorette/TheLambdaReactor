{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Bullet (Bullet(..),moveBullet,expiredBullet) where
import Game.Helper
import GHC.Generics
import Linear.Vector
import Data.Time.Clock
import Debug.Trace


data Bullet = Bullet {uuid :: Int, location :: Location ,velocity :: Float, playerId::Id} deriving (Generic,Show,Eq)

range :: Int
range = 1000


moveBullet :: Bullet -> UTCTime -> Bullet
moveBullet (Bullet uuid location velocity playerId) time =let diff = ((getCurrentMilli time) - uuid)
                                                           in (Bullet  uuid (moveLocation location  ((orientation location) ^* (velocity* (fromIntegral diff)))) velocity playerId)

expiredBullet :: Bullet -> UTCTime -> Bool
expiredBullet (Bullet uuid _ _ _) time = let currentTime = getCurrentMilli time
                                                          in (currentTime - uuid) >  range
