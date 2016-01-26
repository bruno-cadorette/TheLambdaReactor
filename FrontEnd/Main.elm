import Signal
--import Chat
import Window exposing (dimensions)
import Bullet exposing (..)
import Player exposing (..)
import Graphics.Collage

--display : Signal (Int, Int) -> Signal Bullet -> Signal Player -> Graphics.Collage.Element
display = Signal.map3 (\(w,h) b p -> Graphics.Collage.collage w h <| displayBullets (w,h) b ++ displayPlayer (w,h) p)

main =
  display dimensions (bulletSignal dimensions managePlayer) managePlayer
