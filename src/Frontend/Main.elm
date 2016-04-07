module Main where

import Input exposing(..)
import Chat exposing (..)
import Engine exposing (..)
import Player exposing (..)
import Map exposing (displayMap)
import Bullet exposing (..)
import UserInterface exposing (displayUI)

import Signal
import Window exposing (dimensions, width)
import SocketIO exposing (..)
import Task exposing (Task, andThen)
import Dict
import Graphics.Collage
import Graphics.Element as Elem

gameSocket : Task x Socket
gameSocket =
  io "http://localhost:8001" defaultOptions

port playerName : String

port communication : Task a ()
port communication =
  gameSocket `andThen` \socket ->
  chatCommunication socket `andThen`
  always (gameInputCommunication socket) `andThen`
  always (initialMessage socket)

port inputs : Signal (Task x ())
port inputs =
  Signal.mergeMany [(sendMessage playerName gameSocket), (sendShot gameSocket), (sendMovement gameSocket), initializeInput]

display =
  Signal.map3 (\(w,h) chat {player, enemies, bullets} ->
  Graphics.Collage.collage w h <| displayMap w h player.entity.location.position
                                  ++ [displayEntity (w,h) player]
                                  ++ displayEveryone (w,h) (Dict.values enemies)
                                  ++ displayBullets (w,h) bullets
                                  ++ displayUI (w,h) player.entity.hp playerName chat)

main : Signal Elem.Element
main =
  display dimensions (displayChat playerName) <| update currentPlayerId gameStateUpdate
