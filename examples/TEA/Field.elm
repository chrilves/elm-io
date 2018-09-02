module TEA.Field exposing (main)

{-|
@docs main
-}

-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/text_fields.html

import Html exposing (Html, Attribute, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Browser

{-|-}
main: Program () String Msg
main =
  Browser.sandbox { init = "", view = view, update = update }


-- UPDATE

type Msg = NewContent String

update (NewContent content) oldContent =
  content


-- VIEW

view content =
  div []
    [ input ([ placeholder "Text to reverse", onInput NewContent] ++ myStyle) []
    , div myStyle [ text (String.reverse content) ]
    ]

myStyle =
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ] |> List.map (\(k,v) -> style k v)
