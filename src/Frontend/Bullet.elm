module Bullet(displayBullets, Bullet, tickBullets, shootBullet) where
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

type alias Bullet = { position : Point, orientation : Point, speed : Float }

displayBullet (w, h) bullet =
  Graphics.Collage.rotate (getOrientation bullet.position <| mapOrientation w h bullet.orientation)
      <| Graphics.Collage.move (watch "position" <| toTuple bullet.position)
      <| bulletBody

displayBullets (w, h) =
  List.map (displayBullet (w, h))

bulletBody = Graphics.Collage.toForm (Graphics.Element.croppedImage (0, 48) 10 10 "../../resources/sheets/character.png")

tickBullets : List Bullet -> List Bullet
tickBullets = List.map (\b -> {b | position = b.position `add` direction b.orientation b.position, orientation = b.orientation `add` direction b.orientation b.position} |> watchBullet)
  >> List.filter (\b ->
      let p = toRecord b.position
      in p.y < 2000 && p.y > -2000 && p.x < 2000 && p.x > -2000)

shootBullet : Player -> List Bullet -> List Bullet
shootBullet player xs = {position = player.position, orientation = player.orientation, speed = 0.2} :: xs

watchBullet x = watchSummary "playerOrientation" (\x -> toTuple x.orientation) x
