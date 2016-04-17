module Input (gameStateUpdate, gameInputCommunication, currentPlayerId, safeKeyboardPresses, initializeInput, sendMovement, sendShot, sendTest, currentGameMap, emitJSON) where

import Signal
import Signal.Extra exposing (foldps)
import Char exposing (KeyCode)
import Debug exposing (..)
import Keyboard
import Keyboard.Keys exposing (t, enter, Key)
import SocketIO exposing (..)
import Task exposing (Task, andThen)
import Math.Vector2 exposing (..)
import Point exposing (..)
import Math.Vector2 exposing (..)
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Result exposing (Result)
import Dict exposing (Dict)
import GameState exposing (..)
import Window exposing (dimensions)
import Mouse
import Time exposing (every, second)
import Debug exposing (..)

type alias PlayerId = String

gameInputCommunication : Socket -> Task x ()
gameInputCommunication socket =
    on "updateGameState" (decodeSignal jsonDecGameState gameStateMailbox.address) socket `andThen`
    always (on "login" playerIdMailbox.address socket) `andThen`
    always (on "gameMap" (decodeSignal jsonDecGameMap gameMapMailbox.address) socket)

initializeInput : Signal (Task x ())
initializeInput = Signal.map (\x ->
  case x of
    Movement m -> Signal.send movementMailbox.address m
    Typing t   -> Signal.send keyboardInputMailbox.address (log "initializeInput" t)) <| gameInput Keyboard.wasd

gameStateUpdate : Signal GameState
gameStateUpdate = Signal.filterMap identity defaultGameState gameStateMailbox.signal

currentPlayerId : Signal PlayerId
currentPlayerId = Signal.map (log "id") playerIdMailbox.signal

currentGameMap : Signal GameMap
currentGameMap = Signal.filterMap (log "gameMap" ) defaultGameMap gameMapMailbox.signal

safeKeyboardPresses : Signal KeyCode
safeKeyboardPresses = Signal.map (log "safeKeyboardPresses") keyboardInputMailbox.signal

keyboardMovement : Signal Vec2
keyboardMovement = movementMailbox.signal

decodeSignal : Decode.Decoder a -> Signal.Address (Maybe a) -> Signal.Address String
decodeSignal decoder address= Signal.forwardTo address (Decode.decodeString decoder >> logError >> Result.toMaybe)

defaultGameState = { players = Dict.empty, projectiles =  Dict.empty, enemies = [], hits = [] }
defaultGameMap = {size = (0,0), items = [], sprites = []}

gameMapMailbox : Signal.Mailbox (Maybe GameMap)
gameMapMailbox = Signal.mailbox (Just defaultGameMap)

gameStateMailbox : Signal.Mailbox (Maybe GameState)
gameStateMailbox = Signal.mailbox (Just defaultGameState)

playerIdMailbox : Signal.Mailbox PlayerId
playerIdMailbox = Signal.mailbox "null"

keyboardInputMailbox : Signal.Mailbox KeyCode
keyboardInputMailbox = Signal.mailbox 0

movementMailbox : Signal.Mailbox Vec2
movementMailbox = Signal.mailbox (vec2 0 0)

logError : Result String a -> Result String a
logError r =
  case r of
    Ok _ -> r
    Err err -> log err r

type Input = Movement Vec2 | Typing KeyCode

gameInput : Signal {x : Int, y : Int} -> Signal Input
gameInput m =
    let
      movement = Signal.map Movement <| playerInput m
      typing = Signal.map Typing Keyboard.presses
    in
      Signal.filterMap identity (Movement (vec2 0 0)) <| foldps inputHandler (Nothing, True) <| Signal.merge movement typing

inputHandler : Input -> Bool -> (Maybe Input, Bool)
inputHandler input isMovement =
  case input of
    Movement m -> if isMovement then (Just (Movement m), isMovement) else (Nothing, isMovement)
    Typing k   ->
      if isMovement && k == 116 then (Just (Movement (vec2 0 0)), False)
      else if not isMovement && k == enter.keyCode then (Just (Typing enter.keyCode), True)
      else if not isMovement then (Just (Typing k), False)
      else (Nothing, isMovement)

playerInput = Signal.map(\{x, y} -> vec2 (toFloat x) (toFloat y ))

emitFromSignal : (a -> Encode.Value) -> String -> Task x Socket-> Signal a -> Signal (Task x ())
emitFromSignal encodeFunc emitTo socket = Signal.map(\s -> socket `andThen` emitJSON encodeFunc emitTo s)


emitJSON : (a -> Encode.Value) -> String -> a -> Socket -> Task x ()
emitJSON f emitTo a = emit emitTo (Encode.encode 0 <| f a)

--emitMovement : Task x Socket -> Vec2 -> Task x ()
--emitMovement serverSocket v = --serverSocket `andThen` emit "userInput" (log "emit" <| Encode.encode 0 <| jsonEncVec2 v)

normalizedMouseInput = Signal.sampleOn Mouse.clicks <| Signal.map2 (\(w,h) (x,y) -> normalize (mapOrientation w h <| vec2 (toFloat x) (toFloat y))) dimensions Mouse.position

sendMovement : Task x Socket -> Signal (Task x ())
sendMovement s = emitFromSignal jsonEncVec2 "userInput" s movementMailbox.signal

sendShot s = emitFromSignal jsonEncVec2 "userShoot" s normalizedMouseInput

sendTest s = emitFromSignal Encode.float "test" s <| every second
--sendShot : Signal Vec2 -> Signal (Task x ())
--sendShot = sendFromSignal vec2Encoder "shootInput"
