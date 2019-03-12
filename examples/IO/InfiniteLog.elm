{- Example presenting the Batch Effect of IO -}
module Test exposing (..)

import Html exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import IO exposing (..)

type alias Msg = ()
type alias Model = List String

{- Prints one line of log with the provided color -}
log : String -> IO Model Msg
log txt = IO.modify (\l -> txt :: (List.take 9 l))

{- Prints each line of the logs -}
view : Model -> Html Msg
view m =
  div [] [
    h1 [] [text "Logs"],
    ul [] (List.map (\txt -> li [] [text txt]) m)
  ]

test : Int -> IO Model Msg
test n = log (String.fromInt n)
         |> IO.andThen IO.yield
         |> IO.andThen (\_ -> test (n + 1))

main : IO.Program () Model Msg 
main =
  IO.sandbox {
    init = \_ -> ([], test 0),
    view = view >> Html.map IO.pure,
    subscriptions = IO.dummySub
  }