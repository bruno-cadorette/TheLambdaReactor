module Chat where

import Input exposing (safeKeyboardPresses)
import Char exposing (fromCode, KeyCode)
import Debug exposing (..)
import Graphics.Collage as Collage
import Graphics.Element exposing (Element, show, leftAligned, flow, down)
import Keyboard.Keys exposing (enter, backspace, Key)
import List
import Protocol exposing (..)
import Signal
import SocketIO exposing (..)
import String
import Task exposing (Task, andThen)
import Text exposing (Text, color)
import Time exposing (Time)
import Window exposing (dimensions)
import Color exposing (..)

type MessageToSend = SendMessage String | BuildString String
sendToString : MessageToSend -> String
sendToString x =
  case x of
    SendMessage _ -> ""
    BuildString str -> str

----Write a message----
buildWord : Signal MessageToSend
buildWord = Signal.foldp (stringBuilder) (BuildString "") safeKeyboardPresses

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
writtenText = buildWord |> Signal.map (sendToString>>Text.fromString>>color darkBlue>>leftAligned)

timeToClear = Time.every <| 10 * Time.second

receiveMessage : Signal Time -> Signal String -> Signal (List Element)
receiveMessage removeMessage newStr =
  Signal.merge (Signal.map (Text.fromString>>color black>>Just) newStr) (Signal.map (always Nothing) removeMessage)
  |> Signal.foldp (\x xs ->
    case x of
      Just x' -> xs ++ [leftAligned x']
      Nothing -> List.drop 1 xs) []

displayChat : Signal Collage.Form
displayChat =
  Signal.map3(\writting received (w, h) ->
    Collage.group
      [Collage.move ((toFloat w) / 2 - 100, 100 - (toFloat h) / 2) <| Collage.filled white <| Collage.rect 210 200,
      Collage.move ((toFloat w) / 2 - 100, 50 - (toFloat h) / 2) <|
        Collage.toForm <|
          flow down (List.map (Graphics.Element.width 200) (received ++ [writting]))]) writtenText (receiveMessage timeToClear received.signal) Window.dimensions

sendMessage : Task x Socket -> Signal (Task x ())
sendMessage serverSocket = Signal.map (\s ->
  case s of
    SendMessage m -> serverSocket `andThen` emit "sendMessage" m
    BuildString s -> Task.succeed ()) buildWord

-- send a value once at program start
initialMessage : Socket -> Task x ()
initialMessage = emit "newUser" "JOHN CENA"

sendMailbox : Signal.Mailbox String
sendMailbox = Signal.mailbox ""

received : Signal.Mailbox String
received = Signal.mailbox "null"


forwardMessage = Signal.forwardTo received.address
  (\x -> case decodeMessage x of
      Ok {name, body} -> name ++  ": " ++ body
      Err e -> e)

chatCommunication : Socket -> Task.Task a ()
chatCommunication socket =
    on "receiveServerMessage" received.address socket `Task.andThen` \_ ->
    on "receiveMessage" forwardMessage socket
