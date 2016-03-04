module Engine where

import Signal
import Math.Vector2 exposing (..)
import Time exposing(Time, fps)
import Keyboard
import Mouse
import Point exposing(..)
import Graphics.Element exposing (show)
import Player exposing (..)
import Bullet exposing (..)
import Window
import GameState exposing (..)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Map exposing (..)
import Tuple exposing (mapFst)

type alias OutputGameState = {player : OutputEntity, enemies : Dict String OutputEntity, bullets : List Bullet}

initialOutputGameState = {player = initialOutputEntity, enemies = Dict.empty, bullets = []}

fov = 1000

popGet : comparable -> Dict comparable v -> (Maybe v, Dict comparable v)
popGet key dict =
  case Dict.get key dict of
    Just value -> (Just value, Dict.remove key dict)
    Nothing -> (Nothing, dict)


playerInput = Signal.map(\{x, y} -> fromRecord {x = toFloat x, y = toFloat y }) Keyboard.wasd
mouseInput = Signal.map(\(x, y) -> vec2 (toFloat x) (toFloat y)) Mouse.position

--Helper function for filterMap
maybeInsert : comparable -> Maybe a -> Dict comparable a -> Dict comparable a
maybeInsert k v dict =
  case v of
    Just v' -> Dict.insert k v' dict
    Nothing -> dict

{-| Apply a function that may succeed to all values in the dictionary, but only keep the successes.

-}
filterMap : (comparable -> a -> Maybe b) -> Dict comparable a -> Dict comparable b
filterMap f =
  Dict.foldr maybeInsert Dict.empty << Dict.map f

{-| Keep a key-value pair when its key appears in the second dictionary, then use a function to determine the preference.

-}
intersectWith : (a -> b -> c) -> Dict comparable a -> Dict comparable b -> Dict comparable c
intersectWith f d1 d2 =
  filterMap (\k v -> Maybe.map (f v) <| Dict.get k d2 ) d1

enemiesToShow : Vec2 -> Dict String Entity -> Dict String Entity
enemiesToShow playerPosition enemies =
  let newPosition position = position `sub` playerPosition
  in filterMap (\k v ->
    let p = newPosition v.location.position
    in
      if distance (vec2 0 0) p < fov
        then Just (changeEntityPosition p v)
        else Nothing) enemies

--updatePositionsRelativePlayer : Vec2 -> String -> GameState -> --{playerTemp : Entity, bulletsTemp : List Bullet, enemiesTemp : Dict String Entity}
updatePositionsRelativePlayer mousePosition id gameState =
  let (player, enemies) = mapFst (changeEntityOrientation mousePosition << Maybe.withDefault initialEntity) (popGet id gameState.players)
  in {playerTemp = player, bulletsTemp = [], enemiesTemp = enemiesToShow player.location.position enemies}

--I need a different type than the core's version
diff' t1 t2 =
  Dict.foldl (\k v t -> Dict.remove k t) t1 t2

--mergeEvents : Signal Vec2 -> Signal String -> Signal GameState -> Signal OutputGameState
mergeEvents = Signal.map3 updatePositionsRelativePlayer mouseInput

updateEnemies old new =
  let i = intersectWith toOutputEntity old new
      d = Dict.map (\k e -> {entity = e, anim = initialCharacterAnimation}) <| diff' new old
  in Dict.union i d

update : Signal String -> Signal GameState -> Signal OutputGameState
update id gamestate =
  Signal.foldp (\new old -> {player = toOutputEntity old.player new.playerTemp, enemies = updateEnemies old.enemies new.enemiesTemp, bullets = []})
  initialOutputGameState <| mergeEvents id gamestate

--getPlayerPosition : String -> GameState.GameState -> Vec2
--getPlayerPosition id gameState = withDefault (vec2 0 0) <| Dict.get id gameState.players

--overidePlayerOrientation : Vec2 -> Player -> Player -> GameState.GameState
--overidePlayerOrientation orientation id gameState = {gameState | players = Dict.update (Maybe.map (\p -> {p | orientation = orientation})) id gameState }
