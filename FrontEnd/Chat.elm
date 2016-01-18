import Signal
import String
import Keyboard
import Keyboard.Keys exposing (enter, backspace, Key)
import Char exposing (fromCode, KeyCode)
import Text
import Time
import List
import Graphics.Collage as Collage
import Graphics.Element exposing (Element)
import Window
import Html
import Task
import SocketIO exposing (..)
import Html.Events exposing (onWithOptions, keyCode)


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

toText : Signal String -> Signal Collage.Form
toText = Signal.map(\s->Text.fromString s |> Collage.text)

writtenText = Keyboard.presses |> buildWord |> toText

everySecond = Time.every Time.second |> Signal.map (always Nothing)

receiveMessage : Signal String -> Signal (List String)
receiveMessage str =
  Signal.merge (Signal.map Just str) everySecond
  |> Signal.foldp (\x xs ->
    case x of
      Just x' -> xs ++ [x']
      Nothing -> List.drop 1 xs) []

canvas : Signal Element
canvas = Signal.map2(\(w, h) t -> Collage.collage w h [t]) Window.dimensions writtenText

socket = io "http://localhost:8001" defaultOptions

outgoing s = socket `Task.andThen` emit "chatSend" s


receiveMessageMailbox = Signal.mailbox "null"
incoming = socket `Task.andThen` on "myEvent" receiveMessageMailbox.address

--removeDefaultBackspace = onWithOptions "onKeyDown" {defaultOptions | preventDefault = True} keyCode (\_ -> Signal.message  mb.address ())

main = canvas --Signal.map (\c -> Html.main' [removeDefaultBackspace] [Html.fromElement c]) canvas
