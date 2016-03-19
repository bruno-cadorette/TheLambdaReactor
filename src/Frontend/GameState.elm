--THIS IS AN AUTOGENERATED FILES, DO NOT MODIFY
module GameState where

import Json.Decode
import Json.Decode exposing ((:=))
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Math.Vector2 exposing (..)
import Dict exposing (Dict)


jsonDecVec2 = Json.Decode.object2 vec2
  ("x" := Json.Decode.float)
  ("y" := Json.Decode.float)

jsonEncVec2 vector =
  let (x,y) = toTuple vector
  in  Json.Encode.object [("x", Json.Encode.float x), ("y", Json.Encode.float y)]

type alias GameState  =
   { players: (Dict String Entity)
   , projectiles: (List Bullet)
   , enemies: (List Entity)
   , hits: (List Hit)
   }

jsonDecGameState : Json.Decode.Decoder ( GameState )
jsonDecGameState =
   ("players" := Json.Decode.dict (jsonDecEntity)) `Json.Decode.andThen` \pplayers ->
   ("projectiles" := Json.Decode.list (jsonDecBullet)) `Json.Decode.andThen` \pprojectiles ->
   ("enemies" := Json.Decode.list (jsonDecEntity)) `Json.Decode.andThen` \penemies ->
   ("hits" := Json.Decode.list (jsonDecHit)) `Json.Decode.andThen` \phits ->
   Json.Decode.succeed {players = pplayers, projectiles = pprojectiles, enemies = penemies, hits = phits}

jsonEncGameState : GameState -> Value
jsonEncGameState  val =
   Json.Encode.object
   [ ("players", encodeMap (Json.Encode.string) (jsonEncEntity) val.players)
   , ("projectiles", (Json.Encode.list << List.map jsonEncBullet) val.projectiles)
   , ("enemies", (Json.Encode.list << List.map jsonEncEntity) val.enemies)
   , ("hits", (Json.Encode.list << List.map jsonEncHit) val.hits)
   ]



type alias Entity  =
   { hp: Int
   , location: Location
   }

jsonDecEntity : Json.Decode.Decoder ( Entity )
jsonDecEntity =
   ("hp" := Json.Decode.int) `Json.Decode.andThen` \php ->
   ("location" := jsonDecLocation) `Json.Decode.andThen` \plocation ->
   Json.Decode.succeed {hp = php, location = plocation}

jsonEncEntity : Entity -> Value
jsonEncEntity  val =
   Json.Encode.object
   [ ("hp", Json.Encode.int val.hp)
   , ("location", jsonEncLocation val.location)
   ]



type alias Bullet  =
   { uuid: Int
   , location: Location
   , velocity: Float
   , timeStamp: Int
   }

jsonDecBullet : Json.Decode.Decoder ( Bullet )
jsonDecBullet =
   ("uuid" := Json.Decode.int) `Json.Decode.andThen` \puuid ->
   ("location" := jsonDecLocation) `Json.Decode.andThen` \plocation ->
   ("velocity" := Json.Decode.float) `Json.Decode.andThen` \pvelocity ->
   ("timeStamp" := Json.Decode.int) `Json.Decode.andThen` \ptimeStamp ->
   Json.Decode.succeed {uuid = puuid, location = plocation, velocity = pvelocity, timeStamp = ptimeStamp}

jsonEncBullet : Bullet -> Value
jsonEncBullet  val =
   Json.Encode.object
   [ ("uuid", Json.Encode.int val.uuid)
   , ("location", jsonEncLocation val.location)
   , ("velocity", Json.Encode.float val.velocity)
   , ("timeStamp", Json.Encode.int val.timeStamp)
   ]



type alias Hit  =
   { uuid: Int
   , player: Entity
   , bullet: Bullet
   }

jsonDecHit : Json.Decode.Decoder ( Hit )
jsonDecHit =
   ("uuid" := Json.Decode.int) `Json.Decode.andThen` \puuid ->
   ("player" := jsonDecEntity) `Json.Decode.andThen` \pplayer ->
   ("bullet" := jsonDecBullet) `Json.Decode.andThen` \pbullet ->
   Json.Decode.succeed {uuid = puuid, player = pplayer, bullet = pbullet}

jsonEncHit : Hit -> Value
jsonEncHit  val =
   Json.Encode.object
   [ ("uuid", Json.Encode.int val.uuid)
   , ("player", jsonEncEntity val.player)
   , ("bullet", jsonEncBullet val.bullet)
   ]



type alias Location  =
   { position: Vec2
   , orientation: Vec2
   }

jsonDecLocation : Json.Decode.Decoder ( Location )
jsonDecLocation =
   ("position" := jsonDecVec2) `Json.Decode.andThen` \pposition ->
   ("orientation" := jsonDecVec2) `Json.Decode.andThen` \porientation ->
   Json.Decode.succeed {position = pposition, orientation = porientation}

jsonEncLocation : Location -> Value
jsonEncLocation  val =
   Json.Encode.object
   [ ("position", jsonEncVec2 val.position)
   , ("orientation", jsonEncVec2 val.orientation)
   ]



type alias GameMap  =
   { size: (Int, Int)
   , items: (List Int)
   , sprites: (List (Int, Int, Int))
   }

jsonDecGameMap : Json.Decode.Decoder ( GameMap )
jsonDecGameMap =
   ("size" := Json.Decode.tuple2 (,) (Json.Decode.int) (Json.Decode.int)) `Json.Decode.andThen` \psize ->
   ("items" := Json.Decode.list (Json.Decode.int)) `Json.Decode.andThen` \pitems ->
   ("sprites" := Json.Decode.list (Json.Decode.tuple3 (,,) (Json.Decode.int) (Json.Decode.int) (Json.Decode.int))) `Json.Decode.andThen` \psprites ->
   Json.Decode.succeed {size = psize, items = pitems, sprites = psprites}

jsonEncGameMap : GameMap -> Value
jsonEncGameMap  val =
   Json.Encode.object
   [ ("size", (\(v1,v2) -> Json.Encode.list [(Json.Encode.int) v1,(Json.Encode.int) v2]) val.size)
   , ("items", (Json.Encode.list << List.map Json.Encode.int) val.items)
   , ("sprites", (Json.Encode.list << List.map (\(v1,v2,v3) -> Json.Encode.list [(Json.Encode.int) v1,(Json.Encode.int) v2,(Json.Encode.int) v3])) val.sprites)
   ]

