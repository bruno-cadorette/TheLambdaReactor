module Sprites(draw, animator, sprite, crops, Animator, updateSprite, nextImage) where

import Time exposing (..)
import Dict exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

{- Test Values -}

{- Sprites -}
type alias Image =
  { image : Element,
  width : Int,
  height : Int
  }

type alias Sprite =
  { length : Int,
  frames : Dict.Dict Int Image
  }

type alias Crop =
  { top    : Int,
  left   : Int,
  width  : Int,
  height : Int
  }

crops : Crop -> Int -> List Crop
crops crop n =
  case n of
    0 -> []
    _ -> crop :: (crops {crop | left = crop.left + crop.width} (n - 1))

sprite : Element -> List Crop -> Sprite
sprite sheet crops = toSprite (spriteHelper (toForm sheet) (sizeOf sheet) crops)

spriteHelper : Form -> (Int, Int) -> List Crop -> List Image
spriteHelper sheet (width, height) crops =
  case crops of
    [] -> []
    (c::cs) ->
      let translate = (toFloat (-c.left) + (toFloat (width-c.width)/2),
                       toFloat c.top - (toFloat (height - c.height)/2) - 0.5 )
          cropped = collage c.width c.height [move translate sheet]
          image = { image = cropped, width = c.width, height = c.height }
      in image :: (spriteHelper sheet (width, height) cs)

toSprite : List Image -> Sprite
toSprite imgs =
  let n = List.length imgs
      assocs = zip [0..n] imgs
  in { length = n, frames = Dict.fromList assocs }

{- Animation -}
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
    then { animator | current = (animator.current + skip) % (animator.sprite.length), last = t }
    else animator


nextImage : Animator -> Animator
nextImage animator =
  { animator | current = (animator.current + 1) % animator.sprite.length }

{- Draw -}
draw : Animator -> Element
draw animator =
    let maybeElem = Dict.get (animator.current) (animator.sprite.frames) in
    case maybeElem of
      Just img -> img.image
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