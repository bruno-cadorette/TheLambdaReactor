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

mapTileList = List.repeat 600 0
initialMap = { element = flow down (getRows 40 mapTileList), position = Math.Vector2.vec2 0 0 }

type alias Map = { element : Element, position : Point }

--Association of ints to a certain sprite sheet image
tileImages : Dict Int (Int, Int)
tileImages = Dict.fromList (zip [0..1] [(0, 0), (136, 170)])

getCrop : Int -> Element
getCrop index = croppedImage (Maybe.withDefault (0, 0) (get index tileImages)) 32 32 "../../resources/sheets/tiles.png"

getImages : List Int -> List Element
getImages tiles = List.map (\index -> collage 64 64 [(Graphics.Collage.scale 2 (toForm (getCrop index)))]) tiles

getRows : Int -> List Int -> List Element
getRows w tiles = case tiles of
                        [] -> []
                        _ -> (flow right (getImages (List.take w tiles)))::(getRows w (List.drop w tiles))

tickMap : Map -> Point -> Map
tickMap field direction = { field | position = Math.Vector2.vec2 ((getX field.position) - (getX direction)) ((getY field.position) + (getY direction)) }

displayMap : Map -> List Graphics.Collage.Form
displayMap field = [toForm (Graphics.Element.container 300 300 (middleAt (absolute (round (getX field.position))) (absolute (round (getY field.position)))) field.element ) ]

--Utility
zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) -> (x,y) :: zip xs' ys'
    (_, _) -> []

--main = (Graphics.Element.container 300 300 (middleAt (absolute 15)(absolute 15)) (flow down (getRows 40 mapTileList)))
--main = Signal.map (\(x, y) -> Graphics.Collage.collage x y [(Graphics.Collage.filled Color.green sq1), sq2]) Window.dimensions
