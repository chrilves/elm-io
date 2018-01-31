module ExampleHttp exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode

import CmdM exposing (..)

main =
  CmdM.vDomProgram
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { topic : String
  , gifUrl : String
  }


init : String -> (Model, CmdM Msg)
init topic =
  ( Model topic "waiting.gif"
  , getRandomGif topic
  )



-- UPDATE


type alias Msg = Result Http.Error String


update : Msg -> Model -> (Model, CmdM Msg)
update msg model =
  case msg of
    Ok newUrl ->
      (Model model.topic newUrl, CmdM.none)

    Err _ ->
      (model, CmdM.none)


-- VIEW


view : Model -> Html (CmdM Msg)
view model =
  div []
    [ h2 [] [text model.topic]
    -- The view can direcly trigger a command
    , button [ onClick (getRandomGif model.topic)] [ text "More Please!" ]
    , br [] []
    , img [src model.gifUrl] []
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub (CmdM Msg)
subscriptions model =
  Sub.none



-- HTTP


getRandomGif : String -> CmdM Msg
getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    CmdM.lift (Http.send identity (Http.get url decodeGifUrl))


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string
