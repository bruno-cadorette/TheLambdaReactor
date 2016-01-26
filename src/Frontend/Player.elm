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

type alias Player = { position : Point, orientation : Point, direction : Point, anim : Animator }
type Event = Tick Time | Move Point | Orientation Point

initPoint : Math.Vector2.Vec2
initPoint = Math.Vector2.vec2 0 0

changePlayerOrientation : Point -> Player -> Player
changePlayerOrientation p player = { player | orientation = p }

movePlayer : Point -> Player -> Player
movePlayer p player = { player | direction = p, anim = nextImage player.anim }

tickPlayer : Player -> Player
tickPlayer player =
  if player.direction ==  initPoint
    then { player | position = (player.position `add` player.direction) }
    else { player | position = (player.position `add` player.direction), anim = nextImage player.anim }

initialPlayer = { position = origin, direction = origin, orientation = origin, anim = animator (sprite (image 384 48 "../../resources/sheets/character.png") (crops {left = 0, top = 0, width = 48, height = 48 } 8)) (Time.second / 30) }

--displayPlayer : (Int, Int) -> Player -> Element
displayPlayer (w, h) movable =
  [Graphics.Collage.rotate (90 + (getOrientation movable.position movable.orientation))
    <| Graphics.Collage.move (watch "position" (getX movable.position, getY movable.position))
    <| Graphics.Collage.toForm (Sprites.draw movable.anim)]
