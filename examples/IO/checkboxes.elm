module ExampleCheckBoxes exposing (..)

import Html exposing (Html, beginnerProgram, fieldset, input, label, text)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onClick)

import IO exposing (..)

main =
  IO.beginnerVDomProgram { init = optOut, view = view, subscriptions = IO.dummySub }


-- MODEL


type alias Model =
  { notifications : Bool
  , autoplay : Bool
  , location : Bool
  }


optOut : Model
optOut =
  Model True True True

-- UPDATE

type alias Msg = ()

toggleNotifications : IO Model Msg
toggleNotifications = IO.modify (\model -> { model | notifications = not model.notifications } )

toggleAutoplay : IO Model Msg
toggleAutoplay = IO.modify (\model -> { model | autoplay = not model.autoplay })

toggleLocation : IO Model Msg
toggleLocation = IO.modify (\model -> { model | location = not model.location } )

-- VIEW


view : Model -> Html (IO Model Msg)
view model =
  fieldset []
    [ checkbox toggleNotifications "Email Notifications"
    , checkbox toggleAutoplay "Video Autoplay"
    , checkbox toggleLocation "Use Location"
    ]


checkbox : msg -> String -> Html msg
checkbox msg name =
  label
    [ style [("padding", "20px")]
    ]
    [ input [ type_ "checkbox", onClick msg ] []
    , text name
    ]
