import Char exposing (fromCode, KeyCode)
import Debug
import Graphics.Collage as Collage
import Graphics.Element exposing (Element, show)
import Html
import Html.Events exposing (onWithOptions, keyCode)
import Json.Decode as Decode exposing ((:=))
import Keyboard
import Keyboard.Keys exposing (enter, backspace, Key)
import List
import Protocol exposing (..)
import Signal
import SocketIO exposing (..)
import String
import Task exposing (Task, andThen)
import Text exposing (Text)
import Time exposing (Time)
import Window

----Write a message----
buildWord : Signal KeyCode -> Signal String
buildWord = Signal.foldp stringBuilder ""

stringBuilder : KeyCode -> String -> String
stringBuilder key str =
    if key == enter.keyCode then
      ""
    else if key == backspace.keyCode then
      String.dropRight 1 str
    else
      String.append str <| String.fromChar <| fromCode key

writtenText: Signal Text
writtenText = Keyboard.presses |> buildWord |> Signal.map Text.fromString

timeToClear = Time.every <| 10 * Time.second

receiveMessage : Signal Time -> Signal String -> Signal (List Text)
receiveMessage removeMessage newStr=
  Signal.merge (Signal.map (Text.fromString>>Just) newStr) (Signal.map (always Nothing) removeMessage)
  |> Signal.foldp (\x xs ->
    case x of
      Just x' -> xs ++ [x']
      Nothing -> List.drop 1 xs) []


buildText = Signal.map2(\writting received -> Text.join (Text.fromString "\n") (received ++ [writting]) |> Collage.text) writtenText (receiveMessage timeToClear received.signal)

canvas : Signal Element
canvas = Signal.map2(\(w, h) t -> Collage.collage w h [t]) Window.dimensions buildText

socket : Task x SocketIO.Socket
socket = SocketIO.io "http://localhost:8001" SocketIO.defaultOptions

eventName = "example"

-- send a value once at program start
port initial : Task x ()
port initial = socket `andThen` SocketIO.emit "add user" (encodeMessage (Message "allo" "allo" "allo"))


received : Signal.Mailbox String
received = Signal.mailbox "null"

-- set up the receiving of data
port responses : Task x ()
port responses = socket `andThen` SocketIO.on "example1" received.address

port incoming : Task.Task a ()
port incoming = socket `Task.andThen` SocketIO.on "login" received.address

main = canvas
