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

type alias Player = { position : Point, orientation : Point, direction : Point }
type Event = Tick Time | Move Point | Orientation Point

movePlayer : Event -> Player ->  Player
movePlayer event player =
  case event of
    Tick time -> { player | position = (player.position `add` player.direction) }
    Move p -> { player | direction = p, position = (player.position `add` p) }
    Orientation p -> { player | orientation = p }

initialPlayer = { position = origin, direction = origin, orientation = origin }

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
  [Graphics.Collage.rotate (getOrientation movable.position <| mapOrientation w h movable.orientation)
    <| Graphics.Collage.move (watch "position" (getX movable.position, getY movable.position))
    <| playerBody]
