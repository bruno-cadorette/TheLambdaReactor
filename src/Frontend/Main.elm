import Signal
import Keyboard
import Input exposing(..)
import Chat exposing (..)
import Window exposing (dimensions)
import Engine exposing (..)
import Player exposing (..)
import Bullet exposing (..)
import Time exposing (fps)
import SocketIO exposing (..)
import Dict
import Graphics.Collage
import Mouse
import Task exposing (Task, andThen)
import Map exposing (..)

gameSocket : Task x Socket
gameSocket = io "http://localhost:8001" defaultOptions

port communication : Task a ()
port communication =
  gameSocket `andThen` \socket ->
  chatCommunication socket `andThen`
  always (gameInputCommunication socket) `andThen`
  always (initialMessage socket)

port inputs : Signal (Task x ())
port inputs = Signal.mergeMany [(sendMessage gameSocket), (sendShot gameSocket), (sendMovement gameSocket), initializeInput]

--display : Signal (Int, Int) -> Signal Map -> Signal OutputGameState -> Graphics.Collage.Element
display = Signal.map4 (\(w,h) field chat {player, enemies, bullets} ->
  Graphics.Collage.collage w h <|  displayMap player.entity.location.position field ++ [displayEntity (w,h) player] ++ displayEveryone (w,h) (Dict.values enemies) ++ [chat])

main =
  display dimensions (Signal.constant initialMap) displayChat <| update currentPlayerId gameStateUpdate
