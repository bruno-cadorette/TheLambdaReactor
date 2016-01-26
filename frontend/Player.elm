module Player (Player, displayPlayer, managePlayer) where

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

movePlayer : Event -> Player ->  Player
movePlayer event player =
  case event of
    Tick time -> if player.direction ==  initPoint
                  then { player | position = (player.position `add` player.direction) }
                  else { player | position = (player.position `add` player.direction), anim = nextImage player.anim }
    Move p -> { player | direction = p, position = (player.position `add` p), anim = nextImage player.anim }
    Orientation p -> { player | orientation = p }

initialPlayer = { position = origin, direction = origin, orientation = origin, anim = animator (sprite (image 384 48 "../resources/sheets/character.png") (crops {left = 0, top = 0, width = 48, height = 48 } 8)) (Time.second / 30) }

playerBody : Graphics.Collage.Form
playerBody = Graphics.Collage.group
  [Graphics.Collage.filled red (Graphics.Collage.circle 10.0),
  Graphics.Collage.filled black (Graphics.Collage.rect 8 40)]

playerInput : Signal Event
playerInput =
  let wasd  = Signal.map(\p ->Move <| fromRecord {x = toFloat p.x, y = toFloat p.y }) Keyboard.wasd
      time  = Signal.map Tick <| fps 30
      mouse = Signal.map (\(x,y) -> Orientation <| fromRecord { x = toFloat x, y = toFloat y }) position
  in  Signal.mergeMany [time, wasd, mouse]

managePlayer = Signal.foldp movePlayer initialPlayer playerInput

--displayPlayer : (Int, Int) -> Player -> Element
displayPlayer (w, h) movable =
  [Graphics.Collage.rotate (90 + (getOrientation movable.position <| mapOrientation w h movable.orientation))
    <| Graphics.Collage.move (watch "position" (getX movable.position, getY movable.position))
    <| Graphics.Collage.toForm (Sprites.draw movable.anim)]
