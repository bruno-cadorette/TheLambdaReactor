import Signal
import Debug exposing (..)
import Keyboard
import SocketIO exposing (..)
import Task exposing (Task, andThen)
import Math.Vector2 exposing (..)
import Point exposing (..)
import Math.Vector2 exposing (..)
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Result exposing (Result)
import Dict exposing (Dict)

type alias PlayerPosition = { position : Point, orientation : Point }
type alias World = { players : Dict PlayerId PlayerPosition }
type alias PlayerId = String

port serverSocket : Task x Socket
port serverSocket = io "http://localhost:8001" defaultOptions

port communication : Task x ()
port communication = serverSocket `andThen` \socket ->
    on "worldUpdate" decodeSignal socket `andThen` \_ ->
    on "initialConnection" playerIdMailbox.address socket

worldUpdate : Signal World
worldUpdate = Signal.filterMap Result.toMaybe defaultWorld worldMailbox.signal

currentPlayerId : Signal PlayerId
currentPlayerId = playerIdMailbox.signal

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

vec2Decoder = Decode.object2 vec2
  ("x" := Decode.float)
  ("y" := Decode.float)

vec2Encoder vector =
  let (x,y) = toTuple vector
  in  Encode.object [("x", Encode.float x), ("y", Encode.float y)]

playerPositionDecoder =
  Decode.object2 PlayerPosition
    ("position" := vec2Decoder)
    ("orientation" := vec2Decoder)

worldDecoder = Decode.object1 World ("players" := Decode.dict playerPositionDecoder)

logError : Result String a -> Result String a
logError r =
  case r of
    Ok _ -> r
    Err err -> log err r

playerInput = Signal.map(\{x, y} -> vec2 (toFloat x) (toFloat y ))

sendFromSignal : (a -> Encode.Value) -> String -> Signal a -> Signal (Task x ())
sendFromSignal encodeFunc emitTo = Signal.map(\s -> serverSocket `andThen` emit emitTo (Encode.encode 0 <| encodeFunc s))

sendMovement : Signal {x:Int, y:Int} -> Signal (Task x ())
sendMovement = sendFromSignal vec2Encoder "userInput" << playerInput

sendShot : Signal Vec2 -> Signal (Task x ())
sendShot = sendFromSignal vec2Encoder "shootInput"
