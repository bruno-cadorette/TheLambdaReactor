module UserInterface where

import Graphics.Collage as Collage exposing (..)
import Graphics.Element as Elem exposing (..)
import Text exposing (..)
import Color exposing (..)

displayUI : (Int,Int) -> Int -> String -> Element -> List Form
displayUI (w,h) hp playerName chat =
  [ toForm <|
      Elem.container w h Elem.bottomRight <|
        flow right [
          Elem.container (w // 10) (h // 4) Elem.midBottom <|
            flow down <| List.map (\s -> rightAligned <| Text.color white <| Text.fromString s) ["Health: " ++ toString hp, "Player: " ++ playerName],
          chat
        ]
  ]
