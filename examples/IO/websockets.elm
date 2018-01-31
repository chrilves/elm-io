module ExampleWebSocket exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/web_sockets.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket

import IO exposing (..)
import IO.Infix exposing (..)

main =
  IO.beginnerVDomProgram
    { init = init
    , view = view
    , subscriptions = subscriptions
    }


echoServer : String
echoServer =
  "wss://echo.websocket.org"



-- MODEL


type alias Model =
  { input : String
  , messages : List String
  }


init : Model
init = Model "" []

-- UPDATE


type alias Msg = ()

send : IO Model Msg
send =
  IO.get |> IO.andThen (\model ->
    IO.set(Model "" model.messages) <!>
    IO.lift (WebSocket.send echoServer model.input)
  )  

setInput : String -> IO Model Msg
setInput newInput = IO.modify (\model -> { model | input = newInput })

-- SUBSCRIPTIONS

subscriptions : Model -> Sub (IO Model Msg)
subscriptions model =
  WebSocket.listen echoServer (\str ->
    IO.modify (\model -> Model model.input (str :: model.messages))
  )

-- VIEW


view : Model -> Html (IO Model Msg)
view model =
  div []
    [ input [onInput setInput, value model.input] []
    , button [onClick send] [text "Send"]
    , div [] (List.map viewMessage (List.reverse model.messages))
    ]


viewMessage : String -> Html msg
viewMessage msg =
  div [] [ text msg ]
