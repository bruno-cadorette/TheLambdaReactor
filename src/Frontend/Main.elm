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
import Debug exposing (log)
import Bullet exposing (..)

gameSocket : Task x Socket
gameSocket =
  io "http://localhost:8001" defaultOptions

port communication : Task a ()
port communication =
  gameSocket `andThen` \socket ->
  chatCommunication socket `andThen`
  always (gameInputCommunication socket) `andThen`
  always (initialMessage socket)

port inputs : Signal (Task x ())
port inputs =
  Signal.mergeMany [(sendMessage gameSocket), (sendShot gameSocket), (sendMovement gameSocket), sendTest gameSocket, initializeInput]

display =
  Signal.map4 (\(w,h) chat map {player, enemies, bullets} ->
  Graphics.Collage.collage w h <| displayMap player.entity.location.position map
                                  ++ [displayEntity (w,h) player]
                                  ++ displayEveryone (w,h) (Dict.values enemies)
                                  ++ displayBullets (w, h) bullets
                                  ++ [Graphics.Collage.move (150, 0) chat])

main =
  display dimensions displayChat (Signal.map getMap currentGameMap) <| update currentPlayerId gameStateUpdate
