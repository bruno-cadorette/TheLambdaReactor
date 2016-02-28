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
import Debug exposing (..)
import Sprites exposing (..)
import Map exposing (..)

type alias Bullet = { position : Point, orientation : Point, speed : Float, body : Sprites.Animator}

displayBullet (w, h) bullet =
  Graphics.Collage.rotate (getOrientation bullet.position <| mapOrientation w h bullet.orientation)
      <| Graphics.Collage.move (watch "position" <| toTuple bullet.position)
      <| Graphics.Collage.toForm (Sprites.draw bullet.body)

displayBullets (w, h) =
  List.map (displayBullet (w, h))

bulletBody = Sprites.animator (Sprites.sprite "../../resources/sheets/character.png" 10 10 (0, 50) 1) 4 origin

tickBullets : List Bullet -> List Bullet
tickBullets = List.map (\b -> {b | position = b.position `add` scale b.speed (direction b.orientation b.position), orientation = b.orientation `add` direction b.orientation b.position, body = Sprites.update b.body b.position} |> watchBullet)
  >> List.filter (\b ->
      let p = toRecord b.position
      in p.y < 2000 && p.y > -2000 && p.x < 2000 && p.x > -2000)

shootBullet : Player -> Map -> List Bullet -> List Bullet
shootBullet player field xs = {position = origin, orientation = player.orientation, speed = 2, body = bulletBody} :: xs

watchBullet x = watchSummary "playerOrientation" (\x -> toTuple x.orientation) x
