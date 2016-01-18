module Point (Point, mapOrientation, getOrientation, origin) where
import Math.Vector2 exposing (..)
import Debug exposing (..)

type alias Point = Vec2

mapOrientation : Int -> Int -> Point -> Point
mapOrientation w h p =
  fromRecord <| { x = (getX p) - (toFloat <| w // 2), y = (toFloat <| h // 2) - (getY p) }

getOrientation : Point -> Point -> Float
getOrientation p1 p2 =
      watch "orientation" <| (radians <| atan2 (getY p2 - getY p1) (getX p2 - getX p1)  )

origin : Point
origin = fromRecord <| {x=0, y=0}
