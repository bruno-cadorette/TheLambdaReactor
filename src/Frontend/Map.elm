module Map(displayMap, getMap) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Dict exposing (..)
import Point exposing (..)
import Math.Vector2 exposing (..)
import GameState
import Debug exposing (..)

type alias Map = { tiles : Form, position : Point }

mapTileList : List Int
mapTileList =
  List.concat (List.repeat 500 [0,1,1,0])

getMap : GameState.GameMap -> Map
getMap {size, items, sprites} =
  let spriteDict = Dict.fromList <| List.map(\(a,b,c) -> (a, createSprite (b,c))) sprites
  in { tiles = group <| makeMap (snd size) 0 items spriteDict, position = origin }

mapCollage : Int -> Int -> Map -> Element
mapCollage w h map =
  collage w h <| [Graphics.Collage.move (toFloat <| Basics.negate <| w // 2, toFloat <| Basics.negate <| h // 2) map.tiles]

makeMap : Int -> Float -> List Int -> Dict Int Form -> List Form
makeMap w shiftY tiles sprites =
  case tiles of
    [] -> []
    _ -> List.append (makeMapRow 0 shiftY (List.take w tiles) sprites) (makeMap w (shiftY + 32) (List.drop w tiles) sprites)

makeMapRow : Float -> Float -> List Int -> Dict Int Form -> List Form
makeMapRow shiftX shiftY tiles sprites =
  case tiles of
    [] -> []
    (x::xs) -> (Graphics.Collage.move (shiftX, shiftY) <| getSprite x sprites)::(makeMapRow (shiftX + 32) shiftY xs sprites)

createSprite : (Int, Int) -> Form
createSprite position =
  Graphics.Collage.sprite 32 32 position "../../resources/sheets/tiles.png"

getSprite :  Int -> Dict Int Form -> Form
getSprite i sprites =
  Maybe.withDefault (createSprite (0,0)) <| Dict.get (log (toString i) i) sprites

displayMap : Int -> Int -> Vec2 -> Map -> List Form
displayMap w h pos map =
  [Graphics.Collage.move (Basics.negate (getX pos) / 2, Basics.negate (getY pos) / 2) <| toForm <| mapCollage w h map]
