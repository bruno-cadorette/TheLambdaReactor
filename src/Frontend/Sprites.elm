module Sprites(draw, animator, sprite, Animator, updateSprite, nextImage) where

import Time exposing (..)
import Dict exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Math.Vector2 exposing (..)

{- Test Values -}

{- Sprites -}

type alias Sprite =
  { length : Int,
  frames : Dict.Dict Int Element
  }

sprite : String -> Int -> Int -> Int -> Sprite
sprite sheet w h n = toSprite (List.reverse (extractImages sheet w h n))

extractImages : String -> Int -> Int -> Int -> List Element
extractImages sheet w h n = if n == 0
                              then []
                              else (Graphics.Element.croppedImage (((n - 1) * w), 0) w h sheet) :: (extractImages sheet w h (n - 1))

toSprite : List Element -> Sprite
toSprite imgs =
  let n = List.length imgs
      assocs = zip [0..n] imgs
  in { length = n, frames = Dict.fromList assocs }

{- Animation -}
type alias Animator =
  { sprite : Sprite,
  current : Int,
  rate : Int, --Mininmum distnace to change image
  lastPos : Math.Vector2.Vec2 }

animator : Sprite -> Int -> Math.Vector2.Vec2 -> Animator
animator sprite rate pos =
  { sprite = sprite,
  current = 0,
  rate = rate,
  lastPos = pos }

updateSprite : Animator -> Math.Vector2.Vec2 -> Animator
updateSprite animator newPos =
  let dist = Math.Vector2.distance newPos animator.lastPos
      skip = (floor dist) // animator.rate
  in if skip > 0
    then { animator | current = (animator.current + skip) % (animator.sprite.length), lastPos = newPos }
    else animator


nextImage : Animator -> Animator
nextImage animator =
  { animator | current = (animator.current + 1) % animator.sprite.length }

{- Draw -}
draw : Animator -> Element
draw animator =
    let maybeElem = Dict.get (animator.current) (animator.sprite.frames) in
    case maybeElem of
      Just img -> img
      Nothing -> show "Illegal current frame"

{- Utility -}
unMaybe : List( Maybe Element ) -> List Element
unMaybe l =
  case l of
    [] -> []
    (x::xs) -> case x of
      Just y -> y::unMaybe xs
      Nothing -> unMaybe xs

zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) -> (x,y) :: zip xs' ys'
    (_, _) -> []

{- Main -}
--getCurrentImage : Element
--getCurrentImage = draw (updateSprite (animator (sprite (image 384 48 "../resources/sheets/character.png") (crops startCrop 8)) (100 * Time.millisecond)) x))


--main = Signal.map (\x -> draw (updateSprite (animator (sprite (image 384 48 "../resources/sheets/character.png") (crops startCrop 8)) (100 * Time.millisecond)) x)) (every Time.millisecond)
