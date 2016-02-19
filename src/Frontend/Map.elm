module Map(displayMap, tickMap, Map, initialMap) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Window
import Dict exposing (..)
import Time exposing (..)
import Player exposing (..)
import Point exposing (..)
import Math.Vector2 exposing (..)
import Html exposing (..)

mapTileList = List.concat (List.repeat 500 [0,1,1,0])
initialMap = { tiles = makeMap 40 -800 mapTileList, position = Math.Vector2.vec2 0 0 }

type alias Map = { tiles : List Form, position : Point }

{-main =
  collage 1000 1000 (makeMap 40 0 mapTileList)
-}
makeMap w shiftY tiles =
  case tiles of
    [] -> []
    _ -> List.append (makeMapRow -640 shiftY (List.take w tiles)) (makeMap w (shiftY + 32) (List.drop w tiles))

makeMapRow shiftX shiftY tiles =
  case tiles of
    [] -> []
    (x::xs) -> (Graphics.Collage.move (shiftX, shiftY)
                (Graphics.Collage.sprite
                32
                32
                (Maybe.withDefault (0, 0) (Dict.get x crops))
                "../../resources/sheets/tiles.png"))::(makeMapRow (shiftX + 32) shiftY xs)

--Set tile positions
tileDict : List Int -> Int -> Dict (Int, Int) Int
tileDict tiles w =
  let loop a b items = case items of
                          [] -> Dict.empty
                          _ -> Dict.union (tileRow (List.take w items) a b) (loop 0 (b + 1) (List.drop w items))
  in loop 0 0 tiles

--Set tile row
tileRow : List Int -> Int -> Int -> Dict (Int, Int) Int
tileRow tiles x y =
  case tiles of
    [] -> Dict.empty
    (z::zs) -> Dict.union (Dict.singleton (x, y) z) (tileRow zs (x + 1) y)

--Association of ints to a certain sprite sheet image
crops : Dict Int (Int, Int)
crops = Dict.fromList (zip [0..1] [(102, 170), (136, 170)])

tickMap : Map -> Point -> Map
tickMap field direction = { field | position = Math.Vector2.vec2 ((getX field.position) - (getX direction)) ((getY field.position) + (getY direction)) }

displayMap pos field =
  [toForm (container 300 300  (middleAt (absolute (floor (getX pos))) (absolute (floor (getY pos)))) (collage 1280 1600 (field.tiles)))]

--Utility
zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) -> (x,y) :: zip xs' ys'
    (_, _) -> []
