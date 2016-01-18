module Bullet(bulletSignal, displayBullets, Bullet) where
import Mouse
import Time exposing (fps)
import Debug exposing(..)
import Signal
import Graphics.Element exposing (..)
import Graphics.Collage
import Color exposing (red, black, blue)
import List
import Player exposing (Player)
import Point exposing (..)
import Math.Vector2 exposing (..)

type alias Bullet = { position : Point, orientation : Point, speed : Point }

displayBullet (w, h) bullet =
  Graphics.Collage.rotate (getOrientation bullet.position <| mapOrientation w h bullet.orientation)
      <| Graphics.Collage.move (watch "position" <| toTuple bullet.position)
      <| bulletBody

--displayBullets : (Int, Int) -> List Bullet -> Element
displayBullets (w, h) bullets =
  List.map (displayBullet (w, h)) bullets

bulletBody = Graphics.Collage.filled blue (Graphics.Collage.circle 5.0)

moveBullets : List Bullet -> List Bullet
moveBullets = List.map (\b -> {b | position = b.position `add` (direction b.position b.orientation)})

shootEvent : Signal Player -> Signal (Maybe Bullet)
shootEvent player = Signal.sampleOn Mouse.clicks player |> Signal.map (\x -> Just {position = x.position, orientation = x.orientation, speed = fromRecord {x=1,y=2}})

timeUpdate : Signal (Maybe Bullet)
timeUpdate = fps 30 |> Signal.map (always Nothing)

maybeCons : Maybe a -> List a -> List a
maybeCons event xs  =
    case event of
        Just x  -> x::xs
        Nothing -> xs

bulletSignal : Signal Player -> Signal (List Bullet)
bulletSignal player =
  Signal.merge (shootEvent player) timeUpdate
  |> Signal.foldp (\e xs -> maybeCons e xs |> moveBullets) []
