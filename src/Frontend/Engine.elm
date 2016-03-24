module Engine where

import Signal
import Math.Vector2 exposing (..)
import Time exposing(Time, fps)
import Keyboard
import Debug exposing(log)
import Mouse
import Point exposing(..)
import Graphics.Element exposing (show)
import Player exposing (..)
import Bullet exposing (..)
import Window
import DictExtra exposing (..)
import GameState exposing (..)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Map exposing (..)
import Tuple exposing (mapFst)

type alias OutputGameState = {player : OutputEntity, enemies : Dict String OutputEntity, bullets : Dict String OutputBullet}

initialOutputGameState = {player = initialOutputEntity, enemies = Dict.empty, bullets = Dict.empty}

fov = 1000

popGet : comparable -> Dict comparable v -> (Maybe v, Dict comparable v)
popGet key dict =
  case Dict.get key dict of
    Just value -> (Just value, Dict.remove key dict)
    Nothing -> (Nothing, dict)


playerInput = Signal.map(\{x, y} -> fromRecord {x = toFloat x, y = toFloat y }) Keyboard.wasd
mouseInput = Signal.map(\(x, y) -> vec2 (toFloat x) (toFloat y)) Mouse.position

enemiesToShow : Vec2 -> Dict String Entity -> Dict String Entity
enemiesToShow playerPosition enemies =
  let newPosition position = position `sub` playerPosition
  in filterMap (\k v ->
    let p = newPosition v.location.position
    in
      if distance (vec2 0 0) p < fov
        then Just (changeEntityPosition p v)
        else Nothing) enemies

bulletsToShow : Vec2 -> Dict String Bullet -> Dict String Bullet
bulletsToShow playerPosition bullets =
  let newPosition position = position `sub` playerPosition
  in filterMap (\k v ->
    let p = newPosition v.location.position
    in
      if distance (vec2 0 0) p < fov
        then Just (changeBulletPosition p v)
        else Nothing) bullets

--updatePositionsRelativePlayer : Vec2 -> String -> GameState -> --{playerTemp : Entity, bulletsTemp : List Bullet, enemiesTemp : Dict String Entity}
updatePositionsRelativePlayer mousePosition id gameState =
  let (player, enemies) = mapFst (changeEntityOrientation mousePosition << Maybe.withDefault initialEntity) (popGet id gameState.players)
  in {playerTemp = player, bulletsTemp = (log "Projectiles" gameState.projectiles), enemiesTemp = enemiesToShow player.location.position enemies}

--mergeEvents : Signal Vec2 -> Signal String -> Signal GameState -> Signal OutputGameState
mergeEvents =
  Signal.map3 updatePositionsRelativePlayer mouseInput

updateEnemies old new =
  let i = intersectWith toOutputEntity old new
      d = Dict.map (\k e -> {entity = e, anim = initialCharacterAnimation}) <| diff' new old
  in Dict.union i d

updateBullets : Dict String OutputBullet -> Dict String Bullet -> Dict String OutputBullet
updateBullets old new =
  let i = intersectWith toOutputBullet old new
      d = Dict.map (\k b -> { position = b.location.position, orientation = b.location.orientation, body = initialBulletBody }) <| diff' new old
  in Dict.union i d

update : Signal String -> Signal GameState -> Signal OutputGameState
update id gamestate =
  Signal.foldp (\new old -> {player = toOutputEntity old.player new.playerTemp, enemies = updateEnemies old.enemies new.enemiesTemp, bullets = updateBullets (log "oldBullets" old.bullets) (log "newBullets" new.bulletsTemp)})
  initialOutputGameState <| mergeEvents id gamestate

--getPlayerPosition : String -> GameState.GameState -> Vec2
--getPlayerPosition id gameState = withDefault (vec2 0 0) <| Dict.get id gameState.players

--overidePlayerOrientation : Vec2 -> Player -> Player -> GameState.GameState
--overidePlayerOrientation orientation id gameState = {gameState | players = Dict.update (Maybe.map (\p -> {p | orientation = orientation})) id gameState }
