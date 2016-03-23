module Bullet(displayBullets, OutputBullet, updateBullets) where
import Mouse
import Time exposing (fps, fpsWhen, every, second)
import Debug exposing(..)
import Signal
import Graphics.Element exposing (..)
import Graphics.Collage exposing (Form)
import Color exposing (red, black, blue)
import List
import Signal.Time exposing (limitRate)
import Point exposing (..)
import Math.Vector2 exposing (..)
import Debug exposing (..)
import Sprites exposing (..)
import Map exposing (..)
import GameState exposing (..)

type alias OutputBullet = { position : Point, orientation : Point, body : Sprites.Animator}

displayBullet : (Int, Int) -> OutputBullet -> Form
displayBullet (w, h) bullet =
  Graphics.Collage.rotate (getOrientation bullet.position <| mapOrientation w h bullet.orientation)
      <| Graphics.Collage.move (toTuple bullet.position)
      <| Graphics.Collage.toForm (Sprites.draw bullet.body)

updateBullets : List OutputBullet -> List Bullet -> List OutputBullet
updateBullets oldBullets newBullets =
  let updateBullet oldBullet newBullet =
    if hasMoved oldBullet newBullet
      then { position = newBullet.location.position,
             orientation = newBullet.location.orientation,
             body = Sprites.update oldBullet.body newBullet.location.position }
      else { position = newBullet.location.position,
             orientation = newBullet.location.orientation,
             body = oldBullet.body}
  in List.map2 updateBullet oldBullets newBullets

hasMoved : OutputBullet -> Bullet -> Bool
hasMoved old new =
  old.position /= new.location.position

displayBullets : (Int, Int) -> List OutputBullet -> List Form
displayBullets (w, h) bullets =
  List.map (displayBullet (w, h)) bullets
