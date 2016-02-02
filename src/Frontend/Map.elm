module Map(displayMap) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Window
import Dict exposing (..)

--Test Values
--mapTileList = [1, 1, 1, 0, 1, 1, 0, 1, 0 ,0 ,1,1,1,1,1,1,0,1,0,0,0,0,1,1,1,0,1,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,1,0,1,1,1,1,1,1,0,0,0,0,0,0,0]
mapTileList = List.repeat 600 0


--Association of ints to a certain sprite sheet image
tileImages : Dict Int (Int, Int)
tileImages = Dict.fromList (zip [0..1] [(68, 136), (136, 170)])

getCrop : Int -> Element
getCrop index = croppedImage (Maybe.withDefault (0, 0) (get index tileImages)) 32 32 "../../resources/sheets/tiles.png"

getImages : List Int -> List Element
getImages tiles = List.map (\index -> collage 64 64 [(scale 2 (toForm (getCrop index)))]) tiles

getRows : Int -> List Int -> List Element
getRows w tiles = case tiles of
                        [] -> []
                        _ -> (flow right (getImages (List.take w tiles)))::(getRows w (List.drop w tiles))

displayMap : Int -> List Int -> List Graphics.Collage.Form
displayMap w tiles = [toForm(flow down (getRows w tiles))]

--Utility
zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) -> (x,y) :: zip xs' ys'
    (_, _) -> []

--main = displayMap 40 mapTileList
--main = Signal.map (\(x, y) -> Graphics.Collage.collage x y [(Graphics.Collage.filled Color.green sq1), sq2]) Window.dimensions
