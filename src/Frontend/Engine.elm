module Engine (getEvents, EngineEvent(..), GameState, playerInput, mouseInput, run, turn, initialGameState) where

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
--import GameState
import Dict
import Maybe exposing (withDefault)
import Map exposing (..)

type EngineEvent = Tick | Click | Move Point | Orientation Point
type alias OutputGameState = {player : Player, enemies : List Player, bullets = []}

fov = 1000

popGet : comparable -> Dict comparable v -> (Maybe v, Dict comparable v)
popGet key dict =
  case Dict.get key dict of
    Just value -> (Just value, Dict.remove key dict)
    Nothing -> (Nothing, dict)

initialGameState = {player = initialPlayer, bullets = [], field = initialMap}

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
turn event {player, bullets, field} =
  case event of
    Tick -> {player = tickPlayer player, bullets = tickBullets bullets, field = field}
    Click  -> {player = player, bullets = shootBullet player field bullets, field = field}
    Move p -> {player = movePlayer p player, bullets = bullets, field = field}
    Orientation p -> {player = changePlayerOrientation p player, bullets = bullets, field = field}

run : Signal EngineEvent -> Signal GameState
run = Signal.foldp turn initialGameState

update : Vec2 -> String -> GameState -> OutputGameState
update mousePosition id gameState =
  let (player, enemies) = mapEach (Maybe.map <| changePlayerOrientation mousePosition) Dict.values <| popGet id gameState
  playerPosition = Maybe.withDefault (vec 0 0) <| Maybe.map (.position)
  in {player = player, bullets = [], enemies = List.map (\e -> {e | position = position `sub` playerPosition } ) enemies |> List.filter(\e -> distance (vec 0 0) e.position < fov)}


--getPlayerPosition : String -> GameState.GameState -> Vec2
--getPlayerPosition id gameState = withDefault (vec2 0 0) <| Dict.get id gameState.players

--overidePlayerOrientation : Vec2 -> Player -> Player -> GameState.GameState
--overidePlayerOrientation orientation id gameState = {gameState | players = Dict.update (Maybe.map (\p -> {p | orientation = orientation})) id gameState }

main = Signal.map show <| getEvents playerInput mouseInput (fps 30) Window.dimensions Mouse.clicks
