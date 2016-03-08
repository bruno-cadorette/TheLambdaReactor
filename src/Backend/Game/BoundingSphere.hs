module Game.BoundingSphere (BoundingSphere(..), intersecting, intersectingMany,intersectPos) where
  import Linear.V2
  import Data.List

  data BoundingSphere = BoundingSphere {
  position :: V2 Float,
  radius :: Float
}

  intersecting (BoundingSphere (V2 x0 y0) rad0) (BoundingSphere (V2 x1 y1) rad1) =
     (rad0 + rad1) ** 2 >= (x0-x1)**2+(y0-y1)**2

  intersectingMany obj others =
    any (\x -> intersecting obj x) others

  intersectPos :: V2 Float -> V2 Float -> Float -> Float -> Bool
  intersectPos (V2 x0 y0) (V2 x1 y1) rad0 rad1  = intersecting (BoundingSphere (V2 x0 y0) rad0) (BoundingSphere (V2 x1 y1) rad1)
