module CmdM.Random exposing (main)

{-|
@docs main
-}

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/random.html

import Html exposing (..)
import Html.Events exposing (..)
import Random

import CmdM exposing (..)

{-|-}
main: CmdM.Program () Model Msg
main =
  CmdM.element
    { init = \_ -> init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model = { dieFace : Int }


init : (Model, CmdM Msg)
init = (Model 1, CmdM.none)

-- UPDATE

type Msg = NewFace Int

roll : CmdM Msg
roll = CmdM.lift (Random.generate NewFace (Random.int 1 6))

update : Msg -> Model -> (Model, CmdM Msg)
update msg model =
  case msg of
    NewFace newFace ->
      (Model newFace, CmdM.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub a
subscriptions _ = Sub.none

-- VIEW

view : Model -> Html (CmdM Msg)
view model =
  div []
    [ h1 [] [ text (String.fromInt model.dieFace) ]
    , button [ onClick roll ] [ text "Roll" ]
    ]
