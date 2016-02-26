{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Bullet (Bullet(..),moveBullet) where
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics
import Linear.V2
import Linear.Vector
import Control.Lens


data Bullet = Bullet {uuid :: Int, position :: V2 Float, orientation :: V2 Float ,velocity :: Float, timeStamp::Int} deriving (Generic,Show,Eq)

moveBullet (Bullet uuid position orientation velocity timestamp) = (Bullet uuid (position + (orientation ^* velocity)) orientation velocity timestamp)

--Bullet
instance Aeson.ToJSON Bullet

instance Aeson.FromJSON Bullet

instance (Aeson.ToJSON a) => Aeson.ToJSON (V2 a)  where
 toJSON x =
    Aeson.object [ "x"  Aeson..= (x ^._x)
                 , "y"   Aeson..= (x ^._y)
                   ]

instance (Aeson.FromJSON a) => Aeson.FromJSON (V2 a) where
parseJSON (Aeson.Object v) =
   V2     <$> v Aeson..: "x"
          <*> v Aeson..: "y"
