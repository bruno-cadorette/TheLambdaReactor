import String
import List
import Signal
import Graphics.Element exposing (..)

import ElmTest exposing (..)
import Engine exposing (..)
import Math.Vector2 exposing (..)
import Check exposing (..)
import Check.Investigator exposing (..)
import Check.Test exposing (..)
import Random

moveInvestigator =
  let r = rangeInt -1 1
  in tuple3 (r, r, rangeInt 0 100)

claim_product_of_move_equals_number_of_ticks =
  claim
    "If you have a direction and you do n ticks, the final result will be direction * n"
  `that`
    (\(x, y, n) -> (List.foldl turn initialGameState (Move (fromRecord {x = toFloat x, y = toFloat y }) :: List.repeat n Tick)) |> \g -> g.player.position)
  `is`
    (\(x, y, n) -> fromRecord {x = toFloat (x * n), y = toFloat (y * n)})
  `for`
    moveInvestigator


main = show <| quickCheck claim_product_of_move_equals_number_of_ticks
