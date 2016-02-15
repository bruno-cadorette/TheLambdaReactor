module Player (Player, displayPlayer, changePlayerOrientation, movePlayer, tickPlayer, initialPlayer) where

import Mouse exposing (position, clicks)
import Keyboard
import Time exposing (Time, fps)
import Debug exposing(..)
import Signal
import Graphics.Element exposing (..)
import Graphics.Collage
import Color exposing (red, black, blue)
import Signal.Extra exposing (switchWhen)
import Window exposing (dimensions)
import List
import Point exposing (..)
import Math.Vector2 exposing (..)
import Sprites exposing (..)

type alias Player = { position : Point, orientation : Point, direction : Point, anim : Sprites.Animator }
type Event = Tick Time | Move Point | Orientation Point

initPoint : Math.Vector2.Vec2
initPoint = Math.Vector2.vec2 0 0

changePlayerOrientation : Point -> Player -> Player
changePlayerOrientation p player = { player | orientation = p }

movePlayer : Point -> Player -> Player
movePlayer p player = { player | direction = p }

tickPlayer : Player -> Player
tickPlayer player =
  if player.direction == initPoint
    then player
    else { player | anim = updateSprite player.anim player.position }

initialPlayer = { position = origin, direction = origin, orientation = origin, anim = Sprites.animator (Sprites.sprite "../../resources/sheets/character.png" 48 48 8) 4 origin }

--displayPlayer : (Int, Int) -> Player -> Element
displayPlayer (w, h) movable =
  [Graphics.Collage.rotate (getOrientation movable.position movable.orientation - 90)
    <| Graphics.Collage.toForm (Sprites.draw movable.anim)]
