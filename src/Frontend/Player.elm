module Player where

import Mouse exposing (position, clicks)
import Keyboard
import Time exposing (Time, fps)
import Debug exposing(..)
import Signal
import Graphics.Element exposing (..)
import Graphics.Collage
import Color exposing (red, black, blue)
import Signal.Extra exposing (switchWhen)
import Window exposing (dimensions)
import List
import Point exposing (..)
import Math.Vector2 exposing (..)
import GameState exposing (..)
import Sprites exposing (..)

type alias OutputEntity = { entity : Entity, anim : Sprites.Animator }

changeEntityOrientation : Vec2 -> Entity -> Entity
changeEntityOrientation p player =
  let location = player.location
  in {player | location = { location | orientation = p }}

changeEntityPosition : Vec2 -> Entity -> Entity
changeEntityPosition p player =
  let location = player.location
  in {player | location = { location | position = p }}


hasMoved : Entity -> Entity -> Bool
hasMoved old new = old.location.position /= new.location.position

toOutputEntity : OutputEntity -> Entity -> OutputEntity
toOutputEntity old new =
  let animation =
    if hasMoved old.entity new
      then update old.anim new.location.position
      else old.anim
  in {entity = new, anim = animation}

initialLocation = { position = origin, orientation = origin }
initialEntity = { location = initialLocation, hp = 0}
initialCharacterAnimation = Sprites.animator (Sprites.sprite "../../resources/sheets/character.png" 48 48 (0,0) 8) 4 origin
initialOutputEntity = {entity = initialEntity, anim = initialCharacterAnimation}


displayEntity : (Int, Int) -> OutputEntity -> Graphics.Collage.Form
displayEntity (w, h) outputEntity =
  Graphics.Collage.rotate (getOrientation outputEntity.entity.location.position outputEntity.entity.location.orientation - 90)
    <| Graphics.Collage.toForm (Sprites.draw outputEntity.anim)

displayEveryone : ( Int, Int ) -> List OutputEntity -> List Graphics.Collage.Form
displayEveryone p = List.map (displayEntity p)
