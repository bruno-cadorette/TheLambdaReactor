module Game.BoundingSphere (BoundingSphere(..), intersecting, intersectingMany,intersectPos,intersectBoxPos) where
  import Linear.V2
  import Data.List

  data BoundingSphere = BoundingSphere {
  position :: V2 Float,
  radius :: Float
}
  data BoundingBox = BoundingBox {
     aa :: V2 Float,
     bb :: V2 Float
  }

  intersecting :: BoundingSphere -> BoundingSphere -> Bool
  intersecting (BoundingSphere (V2 x0 y0) rad0) (BoundingSphere (V2 x1 y1) rad1) =
     (rad0 + rad1) ** 2 >= (x0-x1)**2+(y0-y1)**2

  intersectingMany obj others =
    any (\x -> intersecting obj x) others

  intersectPos :: V2 Float -> V2 Float -> Float -> Float -> Bool
  intersectPos (V2 x0 y0) (V2 x1 y1) rad0 rad1  = intersecting (BoundingSphere (V2 x0 y0) rad0) (BoundingSphere (V2 x1 y1) rad1)

  intersectBoxPos:: V2 Float -> V2 Float -> Float -> Float -> Bool
  intersectBoxPos (V2 x1 y1) (V2 x2 y2) dif1 dif2 = let pyth = sqrt ((abs(x1 - x2)) ** 2 + (abs(y1 - y2)) ** 2)
                                                      in  pyth < (dif1 + dif2)


  intersectingBox :: BoundingBox -> V2 Float -> Bool
  intersectingBox (BoundingBox (V2 ax ay) (V2 bx by)) (V2 x y)
    | x `elem` [ax .. bx] && y `elem` [by..ay] = True
    | otherwise = False
