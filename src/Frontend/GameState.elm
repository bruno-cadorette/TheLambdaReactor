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
   , ennemies: (List Entity)
   , hits: (List Hit)
   }

jsonDecGameState : Json.Decode.Decoder ( GameState )
jsonDecGameState =
   ("players" := Json.Decode.dict (jsonDecEntity)) `Json.Decode.andThen` \pplayers ->
   ("projectiles" := Json.Decode.list (jsonDecBullet)) `Json.Decode.andThen` \pprojectiles ->
   ("ennemies" := Json.Decode.list (jsonDecEntity)) `Json.Decode.andThen` \pennemies ->
   ("hits" := Json.Decode.list (jsonDecHit)) `Json.Decode.andThen` \phits ->
   Json.Decode.succeed {players = pplayers, projectiles = pprojectiles, ennemies = pennemies, hits = phits}

jsonEncGameState : GameState -> Value
jsonEncGameState  val =
   Json.Encode.object
   [ ("players", encodeMap (Json.Encode.string) (jsonEncEntity) val.players)
   , ("projectiles", (Json.Encode.list << List.map jsonEncBullet) val.projectiles)
   , ("ennemies", (Json.Encode.list << List.map jsonEncEntity) val.ennemies)
   , ("hits", (Json.Encode.list << List.map jsonEncHit) val.hits)
   ]


type Entity  =
    Player {uuid: String, hp: Int, position: Vec2, orientation: Vec2}
    | Enemy {uuid: String, hp: Int, position: Vec2, orientation: Vec2}

jsonDecEntity : Json.Decode.Decoder ( Entity )
jsonDecEntity =
    let jsonDecDictEntity = Dict.fromList
            [ ("Player", Json.Decode.map Player (   ("uuid" := Json.Decode.string) `Json.Decode.andThen` \puuid ->    ("hp" := Json.Decode.int) `Json.Decode.andThen` \php ->    ("position" := jsonDecVec2) `Json.Decode.andThen` \pposition ->    ("orientation" := jsonDecVec2) `Json.Decode.andThen` \porientation ->    Json.Decode.succeed {uuid = puuid, hp = php, position = pposition, orientation = porientation}))
            , ("Enemy", Json.Decode.map Enemy (   ("uuid" := Json.Decode.string) `Json.Decode.andThen` \puuid ->    ("hp" := Json.Decode.int) `Json.Decode.andThen` \php ->    ("position" := jsonDecVec2) `Json.Decode.andThen` \pposition ->    ("orientation" := jsonDecVec2) `Json.Decode.andThen` \porientation ->    Json.Decode.succeed {uuid = puuid, hp = php, position = pposition, orientation = porientation}))
            ]
    in  decodeSumObjectWithSingleField  "Entity" jsonDecDictEntity

jsonEncEntity : Entity -> Value
jsonEncEntity  val =
    let keyval v = case v of
                    Player vs -> ("Player", encodeObject [("uuid", Json.Encode.string vs.uuid), ("hp", Json.Encode.int vs.hp), ("position", jsonEncVec2 vs.position), ("orientation", jsonEncVec2 vs.orientation)])
                    Enemy vs -> ("Enemy", encodeObject [("uuid", Json.Encode.string vs.uuid), ("hp", Json.Encode.int vs.hp), ("position", jsonEncVec2 vs.position), ("orientation", jsonEncVec2 vs.orientation)])
    in encodeSumObjectWithSingleField keyval val



type alias Bullet  =
   { uuid: Int
   , position: Vec2
   , orientation: Vec2
   , velocity: Float
   , timeStamp: Int
   }

jsonDecBullet : Json.Decode.Decoder ( Bullet )
jsonDecBullet =
   ("uuid" := Json.Decode.int) `Json.Decode.andThen` \puuid ->
   ("position" := jsonDecVec2) `Json.Decode.andThen` \pposition ->
   ("orientation" := jsonDecVec2) `Json.Decode.andThen` \porientation ->
   ("velocity" := Json.Decode.float) `Json.Decode.andThen` \pvelocity ->
   ("timeStamp" := Json.Decode.int) `Json.Decode.andThen` \ptimeStamp ->
   Json.Decode.succeed {uuid = puuid, position = pposition, orientation = porientation, velocity = pvelocity, timeStamp = ptimeStamp}

jsonEncBullet : Bullet -> Value
jsonEncBullet  val =
   Json.Encode.object
   [ ("uuid", Json.Encode.int val.uuid)
   , ("position", jsonEncVec2 val.position)
   , ("orientation", jsonEncVec2 val.orientation)
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
