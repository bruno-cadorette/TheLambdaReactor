module Chat where

import Char exposing (fromCode, KeyCode)
import Debug exposing (..)
import Graphics.Collage as Collage
import Graphics.Element exposing (Element, show, leftAligned, flow, down)
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
        log "enter" <| SendMessage str
      else if key == backspace.keyCode then
        BuildString <| String.dropRight 1 str
      else
        BuildString <| String.append str <| String.fromChar <| fromCode key

writtenText: Signal Element
writtenText = buildWord |> Signal.map (sendToString>>Text.fromString>>leftAligned)

timeToClear = Time.every <| 10 * Time.second

decodeMessage : String -> Result Text a
decodeMessage = Result.map (\{name, message} -> Text.join ": " [Text.fromString name, Text.fromString message] ) << Decode.decodeString

receiveMessage : Signal Time -> Signal String -> Signal (List Element)
receiveMessage removeMessage newStr=
  Signal.merge (Signal.map (decodeMessage>>Just) newStr) (Signal.map (always Nothing) removeMessage)
  |> Signal.foldp (\x xs ->
    case x of
      Just x' -> xs ++ [leftAligned x']
      Nothing -> List.drop 1 xs) []

chat : Signal Element
chat = Signal.map2(\writting received -> flow down (received ++ [writting])) writtenText (receiveMessage timeToClear received.signal)

canvas : Signal Element
canvas = Signal.map2(\(w, h) t -> Collage.collage w h [Collage.toForm t]) Window.dimensions chat

socket : Task x SocketIO.Socket
socket = SocketIO.io "http://localhost:8001" SocketIO.defaultOptions

eventName = "example"

port sendMessage : Signal (Task String ())
port sendMessage = Signal.map (\s ->
  case s of
    SendMessage m -> socket `andThen` SocketIO.emit "sendMessage" (encodeMessage (Message m m))
    BuildString s -> Task.succeed ()) buildWord


-- send a value once at program start
port initial : Task x ()
port initial = socket `andThen` SocketIO.emit "add user" (encodeMessage (Message "allo" "allo"))

sendMailbox : Signal.Mailbox String
sendMailbox = Signal.mailbox ""

received : Signal.Mailbox String
received = Signal.mailbox "null"

port receiveMessageFromServer : Task.Task a ()
port receiveMessageFromServer = socket `Task.andThen` SocketIO.on "receiveMessage" received.address

-- set up the receiving of data
port responses : Task x ()
port responses = socket `andThen` SocketIO.on "example1" received.address

port login : Task.Task a ()
port login = socket `Task.andThen` SocketIO.on "login" received.address

main = canvas
