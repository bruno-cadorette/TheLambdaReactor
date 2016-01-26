module Bullet(bulletSignal, displayBullets, Bullet) where
import Mouse
import Time exposing (fps, fpsWhen, every, second)
import Debug exposing(..)
import Signal
import Graphics.Element exposing (..)
import Graphics.Collage
import Color exposing (red, black, blue)
import List
import Signal.Time exposing (limitRate)
import Player exposing (Player)
import Point exposing (..)
import Math.Vector2 exposing (..)
import Debug exposing(..)

type alias Bullet = { position : Point, orientation : Point, speed : Point }
type MouseType = Click | Hold

displayBullet (w, h) bullet =
  Graphics.Collage.rotate (getOrientation bullet.position <| mapOrientation w h bullet.orientation)
      <| Graphics.Collage.move (watch "position" <| toTuple bullet.position)
      <| bulletBody

clicked =  Signal.map (always Click) <| limitRate (1 * second) Mouse.clicks
--clickHold = Signal.map (always Hold) <| fpsWhen 1 Mouse.isDown

canShoot : Signal Bool
canShoot = Mouse.clicks |> Signal.map (always True) -- <| clicked --Signal.merge clicked clickHold{-Signal.map (\i -> False{- watchSummary "i" (always <| i `rem` 30) <| i `rem` 30 == 0 -}) <| Signal.foldp (\click i ->
{-  case click of
    Click -> 1
    Hold  -> i + 1) 0  <| Signal.merge clicked clickHold-}

--displayBullets : (Int, Int) -> List Bullet -> Element
displayBullets (w, h) bullets =
  List.map (displayBullet (w, h)) bullets

bulletBody = Graphics.Collage.filled blue (Graphics.Collage.circle 5.0)

moveBullets : List Bullet -> List Bullet
moveBullets = List.map (\b -> {b | position = b.position `add` direction b.orientation b.position, orientation = b.orientation `add` direction b.orientation b.position} |> lll)
  >> List.filter (\b ->
      let p = toRecord b.position
      in p.y < 2000 && p.y > -2000 && p.x < 2000 && p.x > -2000)



shootEvent : Signal (Int, Int) -> Signal Player -> Signal (Maybe Bullet)
shootEvent dimensions player =
  let shoot = Signal.map2 (\(w, h) x -> Just {position = x.position, orientation = mapOrientation w h x.orientation, speed = fromRecord {x=1,y=2}}) dimensions player
  in Signal.sampleOn canShoot shoot

lll x = watchSummary "playerOrientation" (\x -> toTuple x.orientation) x

timeUpdate : Signal (Maybe Bullet)
timeUpdate = fps 30 |> Signal.map (always Nothing)

maybeCons : Maybe a -> List a -> List a
maybeCons event xs  =
    case event of
        Just x  -> x::xs
        Nothing -> xs

bulletSignal : Signal (Int, Int) -> Signal Player -> Signal (List Bullet)
bulletSignal dimensions player =
  Signal.merge (shootEvent dimensions player) timeUpdate
  |> Signal.foldp (\e xs -> maybeCons e xs |> moveBullets) []
