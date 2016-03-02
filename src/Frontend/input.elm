module Input (gameStateUpdate, gameInputCommunication, currentPlayerId, safeKeyboardPresses, initializeInput) where

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

type alias PlayerId = String

gameInputCommunication : Socket -> Task x ()
gameInputCommunication socket =
    on "gameStateUpdate" decodeSignal socket `andThen` \_ ->
    on "initialConnection" playerIdMailbox.address socket

initializeInput : Signal (Task x ())
initializeInput = Signal.map (\x ->
  case x of
    Movement m -> sendMovement m
    Typing t   -> Signal.send keyboardInputMailbox.address (log "initializeInput" t)) <| gameInput Keyboard.wasd

gameStateUpdate : Signal GameState
gameStateUpdate = Signal.filterMap Result.toMaybe defaultGameState gameStateMailbox.signal

currentPlayerId : Signal PlayerId
currentPlayerId = playerIdMailbox.signal

safeKeyboardPresses : Signal KeyCode
safeKeyboardPresses = Signal.map (log "safeKeyboardPresses") keyboardInputMailbox.signal

movePlayer : Signal PlayerId -> Signal GameState -> Signal Point
movePlayer playerId =
  Signal.foldp (flip sub) (vec2 0 0)
  << Signal.map2 (\id w -> Dict.get id (w.players) |> Maybe.map(.pposition) |> Maybe.withDefault (vec2 0 0)) playerId

decodeSignal : Signal.Address String
decodeSignal = Signal.forwardTo gameStateMailbox.address (Decode.decodeString jsonDecGameState >> logError)

defaultGameState = { players = Dict.empty, projectiles =  [], ennemies = [], hits = [] }

gameStateMailbox : Signal.Mailbox (Result String GameState)
gameStateMailbox = Signal.mailbox (Ok defaultGameState)

playerIdMailbox : Signal.Mailbox PlayerId
playerIdMailbox = Signal.mailbox "null"

keyboardInputMailbox : Signal.Mailbox KeyCode
keyboardInputMailbox = Signal.mailbox 0

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

--sendFromSignal : (a -> Encode.Value) -> String -> Signal a -> Signal (Task x ())
--sendFromSignal encodeFunc emitTo = Signal.map(\s -> serverSocket `andThen` emit emitTo (Encode.encode 0 <| encodeFunc s))

sendMovement : Vec2-> Task x ()
sendMovement v = Task.succeed ()--serverSocket `andThen` emit "userInput" (Encode.encode 0 <| vec2Encoder v)

--sendShot : Signal Vec2 -> Signal (Task x ())
--sendShot = sendFromSignal vec2Encoder "shootInput"
