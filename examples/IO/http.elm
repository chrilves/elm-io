module ExampleHttp exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode

import IO exposing (..)

main =
  IO.vDomProgram
    { init = init "cats"
    , view = view
    , update = IO.dummyUpdate
    , subscriptions = IO.dummySub
    }



-- MODEL


type alias Model =
  { topic : String
  , gifUrl : String
  }

init : String -> (Model, IO Model Msg)
init topic = ( Model topic "waiting.gif" , getRandomGif)



-- UPDATE

type alias Msg = ()

-- VIEW


view : Model -> Html (IO Model Msg)
view model =
  div []
    [ h2 [] [text model.topic]
    , button [ onClick getRandomGif ] [ text "More Please!" ]
    , br [] []
    , img [src model.gifUrl] []
    ]

-- HTTP

getRandomGif : IO Model Msg
getRandomGif =
  IO.get |> IO.andThen (\model ->
    let
      topic = model.topic
      url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
    in
      IO.lift (Http.send identity (Http.get url decodeGifUrl)) |> IO.andThen (\response ->
        case response of
          Ok newUrl -> IO.set (Model topic newUrl)
          Err _     -> IO.none
      ))

decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string