module Map(displayMap) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Dict exposing (..)
import Point exposing (..)
import Math.Vector2 exposing (..)

type alias Map = { tiles : Form, position : Point }

mapTileList : List Int
mapTileList =
  List.concat (List.repeat 500 [0,1,1,0])

initialMap : Map
initialMap =
  { tiles = group (makeMap 40 0 mapTileList), position = origin }

mapCollage : Int -> Int -> Element
mapCollage w h =
  collage w h <| [Graphics.Collage.move (toFloat <| Basics.negate <| w // 2, toFloat <| Basics.negate <| h // 2) initialMap.tiles]

makeMap : Int -> Float -> List Int -> List Form
makeMap w shiftY tiles =
  case tiles of
    [] -> []
    _ -> List.append (makeMapRow 0 shiftY (List.take w tiles)) (makeMap w (shiftY + 32) (List.drop w tiles))

makeMapRow : Float -> Float -> List Int -> List Form
makeMapRow shiftX shiftY tiles =
  case tiles of
    [] -> []
    (x::xs) -> (Graphics.Collage.move (shiftX, shiftY) <| getSprite x)::(makeMapRow (shiftX + 32) shiftY xs)

--Association of ints to a certain sprite sheet image
crops : Dict Int (Int, Int)
crops =
  Dict.fromList (zip [0..1] [(102, 170), (136, 170)])

createSprite : (Int, Int) -> Form
createSprite position =
  Graphics.Collage.sprite 32 32 position "../../resources/sheets/tiles.png"

sprites : Dict Int Form
sprites =
  Dict.map (\k v -> createSprite v) crops

getSprite : Int -> Form
getSprite i =
  Maybe.withDefault (createSprite (0,0)) <| Dict.get i sprites

displayMap : Int -> Int -> Vec2 -> List Form
displayMap w h pos =
  [Graphics.Collage.move (Basics.negate (getX pos) / 2, Basics.negate (getY pos) / 2) <| toForm <| mapCollage w h]

--Utility
zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) -> (x,y) :: zip xs' ys'
    (_, _) -> []
