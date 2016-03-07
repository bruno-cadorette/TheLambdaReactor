{-# LANGUAGE DeriveGeneric #-}
module Game.MapReader(parseMap,mapToExport,GameMap (..)) where
  import Data.Map.Strict as Map
  import Data.Char
  import Linear.V2
  import Control.Lens
  import Data.Aeson
  import GHC.Generics

  data GameMap = GameMap {taille :: (Float,Float), items::[Int]} deriving (Generic,Show, Eq)



  parseMap :: String -> IO (Map (V2 Float) Int)
  parseMap fileName = do

    content <- readFile fileName
    let linesOfFile = lines content
    return (fromList (zipWith2dIndex (fmap (\ x -> fmap  digitToInt  x ) linesOfFile)))


  zipWith2dIndex :: [[a]] -> [((V2 Float), a)]
  zipWith2dIndex xss = [((V2 i j), x) | (j,xs) <- zip [0..] xss
                                   , (i,x)  <- zip [0..] xs
                                   ]

  mapToExport :: Map (V2 Float) Int -> GameMap
  mapToExport gameMap = let maxVec =  (maximum $ Map.keys gameMap)
                         in
                          (GameMap (maxVec ^._x + 1, maxVec ^._y + 1) $ Map.elems gameMap)
