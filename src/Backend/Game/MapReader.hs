{-# LANGUAGE DeriveGeneric #-}
module Game.MapReader(parseMap,mapToExport,GameMap (..)) where

import qualified Data.Map.Strict as Map
import Data.Char
import Data.Bits
import Data.Maybe
import Data.List
import Data.Ord

data GameMap = GameMap {size :: (Int, Int), items::[Int], sprites :: [(Int, Int, Int)]} deriving (Show, Eq)
type MapFromFile = Map.Map (Int, Int) Int

parseMap :: String -> IO MapFromFile
parseMap fileName = do
    content <- readFile fileName
    let linesOfFile = lines content
    return (centerMap (Map.fromList (zipWith2dIndex (fmap (fmap digitToInt) linesOfFile))))


centerMap :: MapFromFile -> MapFromFile
centerMap mff = Map.foldrWithKey (\ (x,y) z  acc -> Map.insert (x-(maxX `div` 2),y-(maxY `div` 2)) z acc) Map.empty mff
    where
      (maxX, maxY) = (maximum $ Map.keys mff)
      (minX, minY) = (minimum $ Map.keys mff)
      middleX = maxX - minX
      middleY = maxY - minY

zipWith2dIndex :: [[a]] -> [((Int, Int), a)]
zipWith2dIndex xss = [((i, j), x) | (j,xs) <- zip [0..] xss
                                   , (i,x)  <- zip [0..] xs
                                   ]

test = [0,0,2,2,0,0,0,4,9,5,8,0,0,4,10,6,8,0,0,0,1,1,0,0]
test2= [15,15,15,15,15,15,15,15,0,0,15,15,15,15,0,0,15,15,15,15,15,15,15,15]

mapToExport :: MapFromFile -> GameMap
mapToExport gameMap = GameMap (x + 1, y + 1) (fmap snd $ sortBy (comparing comp) $ Map.assocs gameMap){-(Map.elems $ neonSprites gameMap)-} [( i, i*32, 0) | i <- [0..15]]
    where
    comp ((x,y), _) = (y,x)
    (maxX, maxY) = (maximum $ Map.keys gameMap)
    (minX, minY) = (minimum $ Map.keys gameMap)
    x = maxX - minX
    y = maxY - minY


neonSprites :: MapFromFile -> MapFromFile
neonSprites gameMap = Map.mapWithKey getSprite gameMap
    where
        getSprite k v = neighborScore $ neighbors k v gameMap

neighbors :: (Int, Int) -> Int -> MapFromFile -> [Int]
neighbors (x, y) v gameMap = fmap (\k-> differentValues $ fromMaybe 1 $ Map.lookup k gameMap) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
    where
        differentValues i = if i == v then 0 else 1

neighborScore :: [Int] -> Int
neighborScore = foldl1 (.|.) . zipWith (*) [8,4,1,2]
