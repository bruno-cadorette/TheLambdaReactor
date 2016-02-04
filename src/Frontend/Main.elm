import Signal
--import Chat
import Window exposing (dimensions)
import Engine exposing (..)
import Player exposing (..)
import Bullet exposing (..)
import Time exposing (fps)
import Graphics.Collage
import Mouse
import Map exposing (..)


--display : Signal (Int, Int) -> Signal Bullet -> Signal Player -> Graphics.Collage.Element
display = Signal.map2 (\(w,h) {player, bullets, field} -> Graphics.Collage.collage w h <| displayMap w h field ++ displayBullets (w,h) bullets ++ displayPlayer (w,h) player)

main =
  display dimensions <| run <| getEvents playerInput mouseInput (fps 30) dimensions Mouse.clicks
