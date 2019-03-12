module Hello exposing (..)

import Html exposing (..)
import Html.Events exposing(..)
import IO exposing (..)

type alias Model  = Int 
type alias Msg = ()

increment : IO Model Msg
increment = IO.modify ((+) 1)

reset : IO Model Msg
reset = IO.set 0

view : Model -> Html (IO Model Msg) 
view m = 
  div [] [
    h1 [] [text "Example of an IO program"],
    p [] [text ("Counter = " ++ (String.fromInt m))],
    button [onClick increment] [text "increment"],
    button [onClick reset] [text "reset"]
  ]

main : IO.Program () Model Msg 
main =
  IO.sandbox {
    init = \_ -> (0, IO.none),
    view = view ,
    subscriptions = IO.dummySub
  }
