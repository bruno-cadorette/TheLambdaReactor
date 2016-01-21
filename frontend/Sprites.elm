
module MySprites where

import Dict exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

{- Test Values -}
testTiles =
  fromList [ (1, (0, 0)),
  (2, (200, 0)),
  (3, (300, 0)),
  (4, (400, 0)),
  (5, (500, 0)),
  (6, (600, 0)),
  (7, (700, 0)),
  (8, (800, 0)),
  (9, (900, 0)) ]
testPos = 1

{- Images -}
cropImage : Dict.Dict Int ( Int, Int ) -> Int -> Maybe Element
cropImage dict tile =
  Maybe.map (\p -> croppedImage p 100 125 "testsheet.png") (get tile dict)

getImage : Maybe Element
getImage =
  cropImage testTiles testPos

imageList : Dict.Dict Int ( Int, Int ) -> List Int -> List (Maybe Element)
imageList b tiles = List.map (\p -> cropImage b p) tiles

{- Animation -}
type alias Sprite = { length : Int, frames : List Element }
type alias Animator =
  { sprite : Sprite,
  current : Int,
  rate : Time,
  last : Time }

animator : Sprite -> Time -> Animator
animator sprite rate =
  { sprite = sprite,
  current = 0,
  rate = rate,
  last = 0 }

updateSprite : Animator -> Time -> Animator
updateSprite animator t =
  let diff = t - animator.last
      skip = floor <| diff / animator.rate
  in if skip > 0
    then { animator | current <- (animator.current + skip) % (animator.sprite.length), last <- t }
    else animator

{- Utility -}
unMaybe : List( Maybe Element ) -> List Element
unMaybe l =
  case l of
    [] -> []
    (x::xs) -> case x of
      Just y -> y::unMaybe xs
      Nothing -> unMaybe xs

{- Main -}
sprite = 

main : Element
main =
