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
initialMap = { sections = makeSections 40 mapTileList, position = Math.Vector2.vec2 0 0 }

type alias Map = { sections : List Element, position : Point }

--Association of ints to a certain sprite sheet image
crops : Dict Int (Int, Int)
crops = Dict.fromList (zip [0..1] [(0, 0), (136, 170)])

images : Dict Int Element
images = Dict.fromList (zip [0..1] (List.map (\index -> (croppedImage (Maybe.withDefault (0, 0) (get index crops)) 32 32 "../../resources/sheets/tiles.png")) [0..1]))

getTile : Int -> Element
getTile index = Maybe.withDefault (show "Invalid index") (Dict.get index images)

getImages : List Int -> List Element
getImages = List.map (\index -> (getTile index))

getRows : Int -> List Int -> List Element
getRows w tiles = case tiles of
                        [] -> []
                        _ -> (flow right (getImages (List.take w tiles)))::(getRows w (List.drop w tiles))

--width of actual map, height of section, list of tiles
getSectionTiles : Int -> Int -> List Int -> List Int
getSectionTiles w h tiles =
  case h of
    0 -> []
    _ -> (List.append (List.take 10 tiles) (getSectionTiles w (h - 1) (List.drop w tiles)))

--Suppose a section is 10 x 5 tiles
makeSections : Int -> List Int -> List Element
makeSections w tiles = [(flow down (getRows 10 (getSectionTiles w 5 tiles)))]

tickMap : Map -> Point -> Map
tickMap field direction = { field | position = Math.Vector2.vec2 ((getX field.position) - (getX direction)) ((getY field.position) + (getY direction)) }

--displayMap : Map -> List Graphics.Collage.Form
displayMap x y field = [ toForm (Graphics.Element.container 300 300 (middleAt (absolute (round (getX field.position))) (absolute (round (getY field.position)))) (Maybe.withDefault (show "error") (List.head field.sections))) ]

--Utility
zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) -> (x,y) :: zip xs' ys'
    (_, _) -> []

--main = (Graphics.Element.container 300 300 (middleAt (absolute 15)(absolute 15)) (flow down (getRows 40 mapTileList)))
--main = Signal.map (\(x, y) -> Graphics.Collage.collage x y [(Graphics.Collage.filled Color.green sq1), sq2]) Window.dimensions
