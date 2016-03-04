{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.Helper (normalize, Location(..), moveLocation, changeOri) where
  import Linear.V2
  import Linear.Vector
  import Control.Lens
  import GHC.Generics


  data Location = Location {position :: V2 Float, orientation :: V2 Float}  deriving (Generic,Show, Eq)


  moveLocation :: Location -> V2 Float -> Location
  moveLocation p pos = p {position = (position p) ^+^ pos}

  changeOri :: Location -> V2 Float -> Location
  changeOri p ori = p {orientation = ori}

  normalize :: V2 Float -> V2 Float
  normalize v = (1/ magnitude) *^ v
                where magnitude = sqrt ((v ^._x) ** 2 + (v ^._y) ** 2)