module IO.Buttons exposing (main)

{-|
@docs main
-}

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html

import Html exposing (div, button, text, Html)
import Html.Events exposing (onClick)

import IO exposing (..)

type alias Model = Int

-- An empty type would mean no message at all!
-- So we use unit at our dummy message type
type alias Msg   = ()

{-|-}
main : IO.Program () Model Msg
main =
  IO.sandbox {
    init = \_ -> (0, IO.none) ,
    view = view,
    subscriptions = IO.dummySub
  }

view : Int -> Html (IO Model Msg)
view model =
  div []
    [ button [ onClick decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick increment ] [ text "+" ]
    ]

increment : IO Model Msg
increment = IO.modify (\x -> x + 1)

decrement : IO Model Msg
decrement = IO.modify (\x -> x - 1)