module Game.Map (Point2d(..), createMap,findNearestWall,module Export) where
  import Data.Trees.KdTree as Export
  import Linear.V2
  import GHC.Float
  import Control.Lens


  data Point2d = Point2d { p2x :: Double, p2y :: Double}
    deriving (Eq, Ord, Show)

  instance Point Point2d where
      dimension _ = 2
      coord 0 p = p2x p
      coord 1 p = p2y p

  v2FloatToPoint2d :: V2 Float -> Point2d
  v2FloatToPoint2d (V2 x y) = (Point2d (float2Double x) (float2Double y))

  toPoint2d :: (Int, Int) -> Point2d
  toPoint2d (x, y) = (Point2d (fromIntegral  x) (fromIntegral  y))
  
  point2dToV2Float :: Point2d -> V2 Float
  point2dToV2Float p = (V2 (double2Float (p2x p)) (double2Float (p2y p)))

  -- on donne les coordo de tous les murs \ (x,y) est le milieu de la case
  createMap :: [(Int, Int)] -> KdTree Point2d
  createMap points =
    let pts2D = fmap toPoint2d points
     in fromList pts2D

  findNearestWall :: V2 Float -> KdTree Point2d -> Maybe (V2 Float)
  findNearestWall x tree =
    let point2d = v2FloatToPoint2d x
    in
      fmap point2dToV2Float (nearestNeighbor tree point2d)
