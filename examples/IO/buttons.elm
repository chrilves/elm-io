module ExampleButtons exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html

import Html exposing (beginnerProgram, div, button, text, Html)
import Html.Events exposing (onClick)

import IO exposing (..)

type alias Model = Int

-- An empty type would mean no message at all!
-- So we use unit at our dummy message type
type alias Msg   = ()

main : IO.Program Never Model Msg
main =
  IO.beginnerVDomProgram { init = 0, view = view, subscriptions = IO.dummySub }

view : Int -> Html (IO Model Msg)
view model =
  div []
    [ button [ onClick decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick increment ] [ text "+" ]
    ]

increment : IO Model Msg
increment = IO.modify (\x -> x + 1)

decrement : IO Model Msg
decrement = IO.modify (\x -> x - 1)