{-# LANGUAGE TemplateHaskell #-}
module ClientSideGeneration (generateGameState) where
import Elm.Module
import Elm.TyRep
import GameState
import Data.Proxy
import Character
import Bullet
import Lib
import Game.MapReader

{-
  This module use elm-bridge to create the json wrapper of our records type for the elm application.
  The output of this file is in GameState.elm
-}

generateGameState :: String
generateGameState =
    moduleHeader "GameState" ++ vec2Json ++ makeModuleContentWithAlterations (defaultAlterations . myAlteration)
    [ DefineElm (Proxy :: Proxy GameState),
      DefineElm (Proxy :: Proxy Entity),
      DefineElm (Proxy :: Proxy Bullet),
      DefineElm (Proxy :: Proxy Hit),
      DefineElm (Proxy :: Proxy Location),
      DefineElm (Proxy :: Proxy GameMap),
      DefineElm (Proxy :: Proxy InitialName),
      DefineElm (Proxy :: Proxy Message)
    ]

myAlteration :: ETypeDef -> ETypeDef
myAlteration = recAlterType $ \t -> case t of
            ETyCon (ETCon "Id") -> ETyCon (ETCon "String")
            -- Do not use V2 with something other than a float if you plan on sending it to Elm
            ETyApp (ETyCon (ETCon "V2")) _ -> ETyCon (ETCon "Vec2")
            _ -> t


--I don't thing there is a better way of doing that, since we're not creating the datatype
--Might break if the package update
vec2Json :: String
vec2Json = unlines ["jsonDecVec2 = Json.Decode.object2 vec2",
    "  (\"x\" := Json.Decode.float)",
    "  (\"y\" := Json.Decode.float)",
    "",
    "jsonEncVec2 vector =",
    "  let (x,y) = toTuple vector",
    "  in  Json.Encode.object [(\"x\", Json.Encode.float x), (\"y\", Json.Encode.float y)]",
    ""]

moduleHeader :: String -> String
moduleHeader moduleName = unlines
     [ "--THIS IS AN AUTOGENERATED FILES, DO NOT MODIFY"
     , "module " ++ moduleName ++ " where"
     , ""
     , "import Json.Decode"
     , "import Json.Decode exposing ((:=))"
     , "import Json.Encode exposing (Value)"
     , "-- The following module comes from bartavelle/json-helpers"
     , "import Json.Helpers exposing (..)"
     , "import Math.Vector2 exposing (..)"
     , "import Dict exposing (Dict)"
     , ""
     , ""
     ]
