import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp

main =
  StartApp.start { model = 0, view = view, update = update }

buttonStyle : Attribute
buttonStyle =
  style [
    ("display", "block"),
    ("width", "200px")
  ]

view address model =
  div [ style [("width", "100%"), ("top", "0px"), ("bottom", "0px"), ("position", "absolute")]]
      [ ul
        [ style [ ("list-style-type", "none"), ("margin", "0"), ("padding", "0"), ("top", "50%"),
                  ("left", "50%"), ("position", "fixed"), ("transform", "translate(-50%, -50%)")]]
        [ button [ buttonStyle, onClick address Decrement ] [ text "Start Game" ],
          button [ buttonStyle, onClick address Increment ] [ text "Choose Server" ]]
      ]

type Action = Increment | Decrement

update action model =
  case action of
    Increment -> model + 2
    Decrement -> model - 1
