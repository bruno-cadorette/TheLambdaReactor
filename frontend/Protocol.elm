module Protocol where
{-| A module encapsulating all protocol knowledge, as well as few other common functions. -}

import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Time exposing (Time)
import Task exposing (Task)

eventName : String
eventName = "chat"

clock : Signal Time
clock = Time.every Time.second

type alias Message =
    { method : String
    , name : String
    , body : String
    }

decodeMessage : String -> Result String Message
decodeMessage =
    Decode.decodeString <| Decode.object3 Message
        ("method" := Decode.string)
        ("name" := Decode.string)
        ("body" := Decode.string)

encodeMessage : Message -> String
encodeMessage {method, name, body} =
    Encode.encode 0 <| Encode.object
        [ ("method", Encode.string method)
        , ("name", Encode.string name)
        , ("body", Encode.string body)
        ]
