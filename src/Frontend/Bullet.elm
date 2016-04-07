module Bullet(displayBullets, OutputBullet, toOutputBullet, initialBulletBody, changeBulletPosition) where

import Point exposing (..)
import Sprites exposing (..)
import Map exposing (..)
import GameState exposing (..)

import Mouse
import Time exposing (fps, fpsWhen, every, second)
import Debug exposing(..)
import Signal
import Graphics.Element exposing (..)
import Graphics.Collage exposing (Form)
import Color exposing (red, black, blue)
import List
import Signal.Time exposing (limitRate)
import Math.Vector2 exposing (..)
import Debug exposing (..)
import Dict exposing (values, Dict)

type alias OutputBullet = { position : Point, orientation : Point, body : Sprites.Animator}

initialBulletBody : Sprites.Animator
initialBulletBody =
  Sprites.animator (Sprites.sprite "../../resources/sheets/character.png" 6 8 (0,50) 1) 1 origin

changeBulletPosition : Vec2 -> Bullet -> Bullet
changeBulletPosition p bullet =
  let location = bullet.location
  in {bullet | location = { location | position = p }}

displayBullet : (Int, Int) -> OutputBullet -> Form
displayBullet (w, h) bullet =
  Graphics.Collage.rotate (getOrientation bullet.position <| mapOrientation w h bullet.orientation)
      <| Graphics.Collage.move (toTuple bullet.position)
      <| Graphics.Collage.toForm (Sprites.draw bullet.body)

toOutputBullet : OutputBullet -> Bullet -> OutputBullet
toOutputBullet oldBullet newBullet =
  let updateBullet old new =
    { position = new.location.position,
      orientation = new.location.orientation,
      body = Sprites.update old.body new.location.position }
  in updateBullet oldBullet newBullet

displayBullets : (Int, Int) -> Dict String OutputBullet -> List Form
displayBullets (w, h) bullets =
  List.map (displayBullet (w, h)) (Dict.values bullets)
