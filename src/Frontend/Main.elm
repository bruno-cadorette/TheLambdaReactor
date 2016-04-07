module Main where

import Input exposing(..)
import Chat exposing (..)
import Engine exposing (..)
import Player exposing (..)
import Map exposing (displayMap, getMap)
import Bullet exposing (..)
import UserInterface exposing (displayUI)

import Signal
import Window exposing (dimensions, width)
import SocketIO exposing (..)
import Task exposing (Task, andThen)
import Dict
import Graphics.Collage
import Graphics.Element as Elem
import GameState exposing (..)

gameSocket : Task x Socket
gameSocket =
  io "http://localhost:8001" defaultOptions

port playerName : String

port communication : Task a ()
port communication =
  gameSocket `andThen` \socket ->
  chatCommunication socket `andThen`
  always (gameInputCommunication socket) `andThen`
  always (emitJSON jsonEncInitialName "newUser" {initialName = playerName} socket)

port inputs : Signal (Task x ())
port inputs =
  Signal.mergeMany [(sendMessage playerName gameSocket), (sendShot gameSocket), (sendMovement gameSocket), initializeInput]

display =
  Signal.map4 (\(w,h) chat map {player, enemies, bullets} ->
  Graphics.Collage.collage w h <| displayMap player.entity.location.position map
                                  ++ [displayEntity (w,h) player]
                                  ++ displayEveryone (w,h) (Dict.values enemies)
                                  ++ displayBullets (w,h) bullets
                                  ++ displayUI (w,h) player.entity.hp playerName chat)

main : Signal Elem.Element
main =
  display dimensions (displayChat playerName) (Signal.map getMap currentGameMap) <| update currentPlayerId gameStateUpdate
