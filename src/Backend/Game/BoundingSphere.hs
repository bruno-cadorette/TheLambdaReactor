module Game.BoundingSphere (BoundingSphere(..), intersecting, intersectingMany,intersectPos,intersectBoxPos,intertest) where
  import Linear.V2
  import Debug.Trace

  data BoundingSphere = BoundingSphere {
  position :: V2 Float,
  radius :: Float
}

  intersecting :: BoundingSphere -> BoundingSphere -> Bool
  intersecting (BoundingSphere (V2 x0 y0) rad0) (BoundingSphere (V2 x1 y1) rad1) =
     (rad0 + rad1) ** 2 >= (x0-x1)**2+(y0-y1)**2

  intersectingMany :: BoundingSphere -> [(a,BoundingSphere)] -> [a]
  intersectingMany obj others = foldr (\ x acc -> if (snd x) then (fst x):acc else acc) [] $ fmap (\(y,x) ->(y, intersecting obj x)) others

  intersectPos :: V2 Float -> V2 Float -> Float -> Float -> Bool
  intersectPos (V2 x0 y0) (V2 x1 y1) rad0 rad1  = intersecting (BoundingSphere (V2 x0 y0) rad0) (BoundingSphere (V2 x1 y1) rad1)

  intersectBoxPos:: V2 Float -> V2 Float -> Float -> Float -> Bool
  intersectBoxPos (V2 x1 y1) (V2 x2 y2) dif1 dif2 = let pyth = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)
                                                      in if pyth < (dif1 + dif2) then (trace ("Bounded " ++ (show pyth) ++ " < " ++ (show $ dif1+dif2)) True) else False
  intertest (V2 x1 y1) (V2 x2 y2) dif1 dif2 = let pyth = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)
                                                in  x1 < x2 && x1 + dif1 > x2 && y1 < y2 && y1 + dif1 > y2
