module ExampleTime exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/time.html

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

import IO exposing (..)

main =
  IO.beginnerVDomProgram
    { init = init
    , view = view
    , subscriptions = subscriptions
    }

-- MODEL


type alias Model = Time

init : Model
init = 0


-- UPDATE

type alias Msg = ()

-- SUBSCRIPTIONS

subscriptions : Model -> Sub (IO Model Msg)
subscriptions model =
  Time.every second IO.set


-- VIEW

view : Model -> Html a
view model =
  let
    angle =
      turns (Time.inMinutes model)

    handX =
      toString (50 + 40 * cos angle)

    handY =
      toString (50 + 40 * sin angle)
  in
    svg [ viewBox "0 0 100 100", width "300px" ]
      [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
      , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
      ]
