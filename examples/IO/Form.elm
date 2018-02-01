module IO.Form exposing (main)

{-|
@docs main
-}
-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/forms.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import IO exposing (..)

{-|-}
main: IO.Program Never Model Msg
main =
  IO.beginnerVDomProgram
    { init = init
    , view = view
    , subscriptions = IO.dummySub
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" "" ""



-- UPDATE

type alias Msg = ()

name : String -> IO Model Msg
name nm = IO.modify (\model -> { model | name = nm })

password : String -> IO Model Msg
password passwd = IO.modify (\model -> { model | password = passwd })

passwordAgain : String -> IO Model Msg
passwordAgain passwd = IO.modify (\model -> { model | passwordAgain = passwd })


-- VIEW


view : Model -> Html (IO Model Msg)
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput name ] []
    , input [ type_ "password", placeholder "Password", onInput password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput passwordAgain ] []
    , viewValidation model
    ]


viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if model.password == model.passwordAgain then
        ("green", "OK")
      else
        ("red", "Passwords do not match!")
  in
    div [ style [("color", color)] ] [ text message ]
