module Protocol where
{-| A module encapsulating all protocol knowledge, as well as few other common functions. -}

import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Time exposing (Time)
import Task exposing (Task)
import Math.Vector2 exposing (..)
import Point exposing (..)
import Dict exposing (Dict)

type alias PlayerPosition = { position : Point, orientation : Point }
type alias GameState = { players : Dict PlayerId PlayerPosition }
type alias PlayerId = String

type alias Message =
    { name : String
    , body : String
    }

decodeMessage : String -> Result String Message
decodeMessage =
    Decode.decodeString <| Decode.object2 Message
        ("name" := Decode.string)
        ("body" := Decode.string)

encodeMessage : Message -> String
encodeMessage {name, body} =
    Encode.encode 0 <| Encode.object
        [ ("name", Encode.string name)
        , ("body", Encode.string body)
        ]

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

gameStateDecoder = Decode.object1 GameState ("players" := Decode.dict playerPositionDecoder)
