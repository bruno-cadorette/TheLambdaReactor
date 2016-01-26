module Engine (getEvents, EngineEvent, GameState, playerInput, mouseInput, run) where

import Signal
import Math.Vector2 exposing (..)
import Time exposing(Time, fps)
import Keyboard
import Mouse
import Point exposing(..)
import Graphics.Element exposing (show)
import Player exposing (..)
import Bullet exposing (..)
import Window

type EngineEvent = Tick | Click | Move Point | Orientation Point
type alias GameState = {player : Player, bullets : List Bullet}

initialGameState = {player = initialPlayer, bullets = []}

getEvents : Signal Point -> Signal Point -> Signal Time -> Signal (Int, Int) -> Signal () -> Signal EngineEvent
getEvents moveVelocity orientation frames dimensions click =
    let moves  = Signal.map Move moveVelocity
        time  = Signal.map (always Tick) frames
        mouse = Signal.map2 (\p (w, h) -> Orientation <| mapOrientation w h p) orientation dimensions
        fire = Signal.map (always Click) click
    in  Signal.mergeMany [time, moves, mouse, fire]


playerInput = Signal.map(\{x, y} -> fromRecord {x = toFloat x, y = toFloat y }) Keyboard.wasd
mouseInput = Signal.map(\(x, y) -> fromRecord {x = toFloat x, y = toFloat y }) Mouse.position

turn : EngineEvent -> GameState -> GameState
turn event {player, bullets} =
  case event of
    Tick   -> {player = tickPlayer player, bullets = tickBullets bullets}
    Click  -> {player = player, bullets = shootBullet player bullets}
    Move p -> {player = movePlayer p player, bullets = bullets}
    Orientation p -> {player = changePlayerOrientation p player, bullets = bullets}

run : Signal EngineEvent -> Signal GameState
run = Signal.foldp turn initialGameState

main = Signal.map show <| getEvents playerInput mouseInput (fps 30) Window.dimensions Mouse.clicks
