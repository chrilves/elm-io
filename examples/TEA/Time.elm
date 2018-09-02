module TEA.Time exposing (main)

{-|
@docs main
-}

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/time.html

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import Browser

{-|-}
main: Program () Model Msg
main =
  Browser.element
    { init = \_ -> init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model = Time.Posix

init : (Model, Cmd Msg)
init = (Time.millisToPosix 0, Cmd.none)

-- UPDATE

type Msg = Tick Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (newTime, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick

-- VIEW

view : Model -> Html Msg
view model =
  let
    angle = turns ((toFloat (Time.posixToMillis model)) / 36000)
    handX = String.fromFloat (50 + 40 * cos angle)
    handY = String.fromFloat (50 + 40 * sin angle)
  in svg  [ viewBox "0 0 100 100", width "300px" ]
          [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
          , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
          ]
