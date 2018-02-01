module ExampleField exposing (..)

-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/text_fields.html

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String

import IO exposing (..)

type alias Model = String
type alias Msg   = ()

main : IO.Program Never Model Msg
main =
  IO.beginnerVDomProgram { init = "", view = view, subscriptions = IO.dummySub }

-- VIEW

view : Model -> Html (IO Model Msg)
view content =
  div []
    [ input [ placeholder "Text to reverse", onInput IO.set , myStyle ] []
    , div [ myStyle ] [ text (String.reverse content) ]
    ]

myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
