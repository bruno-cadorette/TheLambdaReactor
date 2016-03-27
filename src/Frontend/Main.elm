module Main where

import Signal
import Input exposing(..)
import Chat exposing (..)
import Window exposing (dimensions, width)
import Engine exposing (..)
import Player exposing (..)
import SocketIO exposing (..)
import Dict
import Graphics.Collage
import Task exposing (Task, andThen)
import Map exposing (displayMap)
import Debug exposing (log)
import Bullet exposing (..)

gameSocket : Task x Socket
gameSocket =
  io "http://localhost:8001" defaultOptions

port playerName : String

port communication : Task a ()
port communication =
  gameSocket `andThen` \socket ->
  chatCommunication socket playerName `andThen`
  always (gameInputCommunication socket) `andThen`
  always (initialMessage socket)

port inputs : Signal (Task x ())
port inputs =
  Signal.mergeMany [(sendMessage gameSocket), (sendShot gameSocket), (sendMovement gameSocket), initializeInput]

display =
  Signal.map3 (\(w,h) chat {player, enemies, bullets} ->
  Graphics.Collage.collage w h <| displayMap w h player.entity.location.position
                                  ++ [displayEntity (w,h) player]
                                  ++ displayEveryone (w,h) (Dict.values enemies)
                                  ++ displayBullets (w, h) bullets
                                  ++ [chat])

main =
  display dimensions displayChat <| update currentPlayerId gameStateUpdate
