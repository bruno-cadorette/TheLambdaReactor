{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.Helper (normalize, Location(..), moveLocation, changeOri,getSocketId,add,moveWithFPS,divide) where
  import Linear.V2
  import Linear.Vector
  import Control.Lens
  import GHC.Generics
  import Data.ByteString.Char8
  import Data.Text.Encoding
  import Data.Text
  import Network.SocketIO
  type Id = Text
  --TODO move to helper
  toId :: ByteString -> Id
  toId = decodeUtf8

  getSocketId :: Socket -> Id
  getSocketId  s = toId $ socketId s

  data Location = Location {position :: V2 Float, orientation :: V2 Float}  deriving (Generic,Show, Eq)

  add :: Location -> Location -> Location
  add l1 l2 = changeOri (moveLocation l1 $ position l2) $orientation l2

  moveWithFPS :: Location -> Location -> Location
  moveWithFPS l1 l2 = changeOri (moveLocation l1 $ orientation l2) $orientation l2

  moveLocation :: Location -> V2 Float -> Location
  moveLocation p pos = p {position = (position p) ^+^ pos}

  changeOri :: Location -> V2 Float -> Location
  changeOri p ori = p {orientation = ori}

  normalize :: V2 Float -> V2 Float
  normalize (V2 0 0) = (V2 0 0)
  normalize v = (1/ magnitude) *^ v
                where magnitude = sqrt ((v ^._x) ** 2 + (v ^._y) ** 2)

  divide :: Float -> V2 Float -> V2 Float
  divide 0 _ = (V2 0 0)
  divide z (V2 x y) = (V2 (x/z) (y/z))
