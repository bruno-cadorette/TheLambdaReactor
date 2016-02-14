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

mapTileList = List.concat (List.repeat 200 [0,1,1,0])
initialMap = { tiles = tileDict mapTileList 40, position = Math.Vector2.vec2 0 0 }

type alias Map = { tiles : Dict (Int, Int) Int, position : Point }

--Set tile positions
tileDict : List Int -> Int -> Dict (Int, Int) Int
tileDict tiles w =
  let doWork a b items = case items of
                          [] -> Dict.empty
                          _ -> Dict.union (tileRow (List.take w items) a b) (doWork 0 (b + 1) (List.drop w items))
  in doWork 0 0 tiles

--Set tile row
tileRow : List Int -> Int -> Int -> Dict (Int, Int) Int
tileRow tiles x y =
  case tiles of
    [] -> Dict.empty
    (z::zs) -> Dict.union (Dict.singleton (x, y) z) (tileRow zs (x + 1) y)

--Association of ints to a certain sprite sheet image
crops : Dict Int (Int, Int)
crops = Dict.fromList (zip [0..1] [(102, 170), (136, 170)])

images : Dict Int Element
images = Dict.fromList (zip [0..1] (List.map (\index -> (croppedImage (Maybe.withDefault (0, 0) (get index crops)) 32 32 "../../resources/sheets/tiles.png")) [0..1]))

--Window size = 80x60
getCurrentTiles : Int -> Int -> Int -> Int -> Dict (Int, Int) Int -> Element
getCurrentTiles w h x y tiles =
  (flow down (getRows w h x y tiles))

getRows : Int -> Int -> Int -> Int -> Dict (Int, Int) Int -> List Element
getRows w h x y tiles =
  case h of
    0 -> []
    _ -> (flow right (getRow w x y tiles))::(getRows w (h - 1) x (y + 1) tiles)

getRow : Int -> Int -> Int -> Dict (Int, Int) Int -> List Element
getRow w x y tiles =
  case w of
    0 -> []
    _ -> (getTile (Maybe.withDefault 0 (Dict.get (x, y) tiles)))::(getRow (w - 1) (x + 1) y tiles)

getTile : Int -> Element
getTile index = Maybe.withDefault (show "Invalid index") (Dict.get index images)

tickMap : Map -> Point -> Map
tickMap field direction = { field | position = Math.Vector2.vec2 ((getX field.position) - (getX direction)) ((getY field.position) + (getY direction)) }

sectionByPosition : (Int, Int) -> Map -> Int
sectionByPosition (x, y) field = (x // 32)

--displayMap : Map -> List Graphics.Collage.Form
displayMap pos field = [ toForm (Graphics.Element.container 320 192 (middleAt (absolute (160 - ((floor (getX pos)) % 32))) (absolute (96 + ((floor (getY pos)) % 32)))) (getCurrentTiles 12 8 (((floor (getX pos)) // 32) - 6) (((floor (getY pos)) // 32) - 4) field.tiles))]

--Utility
zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) -> (x,y) :: zip xs' ys'
    (_, _) -> []

--main = (Graphics.Element.container 300 300 (middleAt (absolute 15)(absolute 15)) (flow down (getRows 40 mapTileList)))
--main = Signal.map (\(x, y) -> Graphics.Collage.collage x y [(Graphics.Collage.filled Color.green sq1), sq2]) Window.dimensions
