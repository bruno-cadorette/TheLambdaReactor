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




--display : Signal (Int, Int) -> Signal Bullet -> Signal Player -> Graphics.Collage.Element
display = Signal.map2 (\(w,h) {player, bullets, field} -> Graphics.Collage.collage w h <| displayMap player.position field ++ displayBullets (w,h) bullets ++ displayPlayer (w,h) player)


main =
  display dimensions <| run <| getEvents playerInput mouseInput (fps 30) dimensions Mouse.clicks

--main =
--  display dimensions <| run gameStateUpdate
