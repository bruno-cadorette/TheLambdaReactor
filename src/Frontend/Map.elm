module Map(displayMap, getMap) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Dict exposing (..)
import Point exposing (..)
import Math.Vector2 exposing (..)
import GameState

import Debug exposing (..)

type alias Map = { tiles : Form, position : Point, size : (Int, Int) }

exampleMap = { size = (6,4), items = [15,15,15,15,15,15,15,15,0,0,15,15,15,15,0,0,15,15,15,15,15,15,15,15], sprites = [(0,0,0),(1,32,0),(2,64,0),(3,96,0),(4,128,0),(5,160,0),(6,192,0),(7,224,0),(8,256,0),(9,288,0),(10,320,0),(11,352,0),(12,384,0),(13,416,0),(14,448,0),(15,480,0)] }

getMap : GameState.GameMap -> Map
getMap {size, items, sprites} =
  let spriteDict = Dict.fromList <| List.map(\(a,b,c) -> (a, createSprite (b,c))) sprites
  in { tiles = group <| makeMap (fst size) 16 items spriteDict, position = origin, size = (32 * fst size , 32 * snd size) }

mapCollage : (Int, Int) -> Map -> Element
mapCollage (w, h) map =
  collage w h <| [Graphics.Collage.move (toFloat <| Basics.negate <| w // 2, toFloat <| Basics.negate <| h // 2) map.tiles]

makeMap : Int -> Float -> List Int -> Dict Int Form -> List Form
makeMap w shiftY tiles sprites =
  case tiles of
    [] -> []
    _ -> List.append (makeMapRow 16 shiftY (List.take w tiles) sprites) (makeMap w (shiftY + 32) (List.drop w tiles) sprites)

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
  Maybe.withDefault (createSprite (0,0)) <| Dict.get i sprites

displayMap : Vec2 -> Map -> List Form
displayMap pos _ =
  let map = getMap exampleMap
  in [Graphics.Collage.move (Basics.negate (getX pos), Basics.negate (getY pos)) <| toForm <| mapCollage map.size map]
