module Example where

import Graphics.Element exposing (show)
import Task exposing (Task, andThen)
import String
import Time
import SocketIO
import Protocol exposing (..)

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


messages : Signal (List Message)
messages =
    Signal.map (decodeMessage>>Result.toMaybe) received.signal
    |> Signal.dropRepeats
    |> Signal.foldp (\mx xs -> case mx of
        Just x -> x::xs
        Nothing -> xs) []

-- send many values (with a 1-second throttle)

main = Signal.map show received.signal
