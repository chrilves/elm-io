module ExampleRandom exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/random.html

import Html exposing (..)
import Html.Events exposing (..)
import Random

import IO exposing (..)

main =
  IO.beginnerVDomProgram
    { init = init
    , view = view
    , subscriptions = IO.dummySub
    }


-- MODEL


type alias Model =
  { dieFace : Int
  }

init : Model
init = Model 1

-- UPDATE


type alias Msg = ()

roll : IO Model Msg
roll =
 IO.lift (Random.generate identity (Random.int 1 6)) |> IO.andThen (
    \newFace -> IO.set (Model newFace)
  )

-- VIEW


view : Model -> Html (IO Model Msg)
view model =
  div []
    [ h1 [] [ text (toString model.dieFace) ]
    , button [ onClick roll ] [ text "Roll" ]
    ]