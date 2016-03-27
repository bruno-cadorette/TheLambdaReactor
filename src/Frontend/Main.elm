module Main where

import Signal
import Input exposing(..)
import Chat exposing (..)
import Window exposing (dimensions)
import Engine exposing (..)
import Player exposing (..)
import SocketIO exposing (..)
import Dict
import Graphics.Collage
import Task exposing (Task, andThen)
import Map exposing (..)

gameSocket : Task x Socket
gameSocket =
  io "http://localhost:8001" defaultOptions

port communication : Task a ()
port communication =
  gameSocket `andThen` \socket ->
  chatCommunication socket `andThen`
  always (gameInputCommunication socket) `andThen`
  always (initialMessage socket) `andThen`
  always (emit "test1" "test1" socket) `andThen`
  always (emit "test2" "test2" socket)

port inputs : Signal (Task x ())
port inputs =
  Signal.mergeMany [(sendMessage gameSocket), (sendShot gameSocket), (sendMovement gameSocket), initializeInput]

display =
  Signal.map3 (\(w,h) chat {player, enemies, bullets} ->
  Graphics.Collage.collage w h <| displayMap w h player.entity.location.position
                                  ++ [displayEntity (w,h) player]
                                  ++ displayEveryone (w,h) (Dict.values enemies)
                                  ++ displayBullets (w, h) bullets
                                  ++ [Graphics.Collage.move (150, 0) chat])

main =
  display dimensions displayChat (Signal.map getMap currentGameMap) <| update currentPlayerId gameStateUpdate
