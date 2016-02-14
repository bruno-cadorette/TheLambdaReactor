import Json.Decode exposing (..)
import Graphics.Element exposing (..)

main = show <| decodeString (dict int) "{\"a\":2,\"b\":4}"