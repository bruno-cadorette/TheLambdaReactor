module Map(displayMap, Map, initialMap) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Dict exposing (..)
import Player exposing (..)
import Point exposing (..)
import Math.Vector2 exposing (..)

mapTileList = List.concat (List.repeat 500 [0,1,1,0])
initialMap = { tiles = group (makeMap 40 -800 mapTileList), position = origin }

type alias Map = { tiles : Form, position : Point }

makeMap w shiftY tiles =
  case tiles of
    [] -> []
    _ -> List.append (makeMapRow -640 shiftY (List.take w tiles)) (makeMap w (shiftY + 32) (List.drop w tiles))

makeMapRow shiftX shiftY tiles =
  case tiles of
    [] -> []
    (x::xs) -> (Graphics.Collage.move (shiftX, shiftY) <| getSprite x)::(makeMapRow (shiftX + 32) shiftY xs)

--Association of ints to a certain sprite sheet image
crops : Dict Int (Int, Int)
crops = Dict.fromList (zip [0..1] [(102, 170), (136, 170)])

createSprite position = Graphics.Collage.sprite 32 32 position "../../resources/sheets/tiles.png"

getSprite : Int -> Form
getSprite i = Maybe.withDefault (createSprite (0,0)) <| Dict.get i sprites

sprites : Dict Int Form
sprites = Dict.map (\k v -> createSprite v) crops

displayMap pos field =
  [toForm (container 1024 728  (middleAt (absolute (floor (getX (Math.Vector2.negate pos)))) (absolute (floor (getY pos)))) (collage 1280 1600 [field.tiles]))]

--Utility
zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) -> (x,y) :: zip xs' ys'
    (_, _) -> []
