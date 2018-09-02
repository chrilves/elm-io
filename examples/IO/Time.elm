module IO.Time exposing (main)

{-|
@docs main
-}

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/time.html

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

import IO exposing (..)

{-|-}
main: IO.Program () Model Msg
main =
  IO.sandbox
    { init = \_ -> (init, IO.none)
    , view = view
    , subscriptions = subscriptions
    }

-- MODEL


type alias Model = Time.Posix

init : Model
init = Time.millisToPosix 0


-- UPDATE

type alias Msg = ()

-- SUBSCRIPTIONS

subscriptions : Model -> Sub (IO Model Msg)
subscriptions model =
  Time.every 1000 IO.set


-- VIEW

view : Model -> Html a
view model =
  let angle = turns ((toFloat (Time.posixToMillis model)) / 36000)
      handX = String.fromFloat (50 + 40 * cos angle)
      handY = String.fromFloat (50 + 40 * sin angle)
  in svg  [ viewBox "0 0 100 100", width "300px" ]
          [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
          , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
          ]
