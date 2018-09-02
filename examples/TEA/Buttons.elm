module TEA.Buttons exposing (main)

{-|
@docs main
-}

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html

import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import Browser

{-|-}
main: Program () Int Msg
main =
  Browser.sandbox { init = 0, view = view, update = update }


view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]


type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1