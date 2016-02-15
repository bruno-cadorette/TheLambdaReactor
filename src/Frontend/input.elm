module Input (worldUpdate, currentPlayerId, gameSocket, safeKeyboardPresses) where

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
import Protocol exposing (..)

gameSocket = serverSocket

port serverSocket : Task x Socket
port serverSocket = io "http://localhost:8001" defaultOptions

port communication : Task x ()
port communication = serverSocket `andThen` \socket ->
    on "worldUpdate" decodeSignal socket `andThen` \_ ->
    on "initialConnection" playerIdMailbox.address socket

port input : Signal (Task x ())
port input = Signal.map (\x ->
  case x of
    Movement m -> sendMovement m
    Typing t   -> Signal.send keyboardInputMailbox.address t) <| gameInput Keyboard.wasd

worldUpdate : Signal World
worldUpdate = Signal.filterMap Result.toMaybe defaultWorld worldMailbox.signal

currentPlayerId : Signal PlayerId
currentPlayerId = playerIdMailbox.signal

safeKeyboardPresses : Signal KeyCode
safeKeyboardPresses = keyboardInputMailbox.signal

movePlayer : Signal PlayerId -> Signal World -> Signal Point
movePlayer playerId =
  Signal.foldp (flip sub) (vec2 0 0)
  << Signal.map2 (\id w -> Dict.get id (w.players) |> Maybe.map(.position) |> Maybe.withDefault (vec2 0 0)) playerId

decodeSignal : Signal.Address String
decodeSignal = Signal.forwardTo worldMailbox.address (Decode.decodeString worldDecoder >> logError)

defaultWorld = { players = Dict.empty }

worldMailbox : Signal.Mailbox (Result String World)
worldMailbox = Signal.mailbox (Ok defaultWorld)

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
      if isMovement && k == t.keyCode then (Just (Typing 0), False)
      else if not isMovement && k == enter.keyCode then (Just (Typing enter.keyCode), True)
      else (Nothing, isMovement)

playerInput = Signal.map(\{x, y} -> vec2 (toFloat x) (toFloat y ))

sendFromSignal : (a -> Encode.Value) -> String -> Signal a -> Signal (Task x ())
sendFromSignal encodeFunc emitTo = Signal.map(\s -> serverSocket `andThen` emit emitTo (Encode.encode 0 <| encodeFunc s))

sendMovement : Vec2-> Task x ()
sendMovement v = serverSocket `andThen` emit "userInput" (Encode.encode 0 <| vec2Encoder v)

sendShot : Signal Vec2 -> Signal (Task x ())
sendShot = sendFromSignal vec2Encoder "shootInput"
