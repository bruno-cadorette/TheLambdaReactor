import Char exposing (fromCode, KeyCode)
import Debug exposing (..)
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

type MessageToSend = SendMessage String | BuildString String
sendToString : MessageToSend -> String
sendToString x =
  case x of
    SendMessage _ -> ""
    BuildString str -> str

----Write a message----
buildWord : Signal MessageToSend
buildWord = Signal.foldp (stringBuilder) (BuildString "") (Keyboard.presses)

stringBuilder : KeyCode -> MessageToSend -> MessageToSend
stringBuilder key m =
    let str = sendToString m
    in
      if key == enter.keyCode then
        SendMessage str
      else if key == backspace.keyCode then
        BuildString <| String.dropRight 1 str
      else
        BuildString <| String.append str <| String.fromChar <| fromCode key

writtenText: Signal Text
writtenText = buildWord |> Signal.map (sendToString>>Text.fromString)

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

port sendMessage : Signal (Task String ())
port sendMessage = Signal.map (\s ->
  case s of
    SendMessage m -> socket `andThen` SocketIO.emit "sendMessage" (encodeMessage (Message m m m))
    BuildString s -> Task.succeed ()) buildWord


-- send a value once at program start
port initial : Task x ()
port initial = socket `andThen` SocketIO.emit "add user" (encodeMessage (Message "allo" "allo" "allo"))

sendMailbox : Signal.Mailbox String
sendMailbox = Signal.mailbox ""

received : Signal.Mailbox String
received = Signal.mailbox "null"

-- set up the receiving of data
port responses : Task x ()
port responses = socket `andThen` SocketIO.on "example1" received.address

port incoming : Task.Task a ()
port incoming = socket `Task.andThen` SocketIO.on "login" received.address

main = canvas
