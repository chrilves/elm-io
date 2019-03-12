{- Example presenting the Batch Effect of IO -}
module Batch exposing (..)

import Html exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import IO exposing (..)
import Task
import Process
import Time

{- We do not need messages! Any type would
   be fine as messenges so we take the simplest
   one -}
type alias Msg = ()

{- We need colored logs -}
type alias Color = String

{- This example is a logging application.
   Each item of the list is a logging line. -}
type alias Model = List (Color, String)


{- The Time.now task wrapped in IO -}
now : IO model Time.Posix
now = IO.lift (Task.perform identity Time.now)

{- The Time.here task wrapped in IO -}
here : IO model Time.Zone
here = IO.lift (Task.perform identity Time.here)

{- Prints one line of log with the provided color -}
log : Color -> String -> IO Model Msg
log color txt =
  here |> IO.andThen (\zone -> -- We get the current zone
  now |> IO.andThen (\posix -> -- and the current time
    let h  = String.fromInt (Time.toHour zone posix) -- current hour
        m  = String.fromInt (Time.toMinute zone posix) -- current minutes
        s  = String.fromInt (Time.toSecond zone posix) -- current seconds
        ms = String.fromInt (Time.toMillis zone posix) -- current millis
        line  = "[" ++ h ++ ":" ++ m ++ ":" ++ s ++ "." ++ ms ++ "] " ++ txt
    in IO.modify (\l -> l ++ [(color, line)]) -- add the line at the end of the list
  ))

{- Prints each line of the logs -}
view : Model -> Html (IO Model Msg)
view m =
  div [] [
    button [onClick (IO.set [] |> IO.seq test)] [text "Reset"],
    h1 [] [text "Logs"],
    ul [] (List.map (\(color,txt) -> li [style "color" color] [text txt]) m)
  ]

{- Wait for duration milliseconds, then returns msg -}
wait : Float -> msg -> IO model msg
wait duration msg =
  IO.lift (Task.perform (\_ -> msg) (Process.sleep duration))

{- Simulates a task:
     - log the start of the task
     - do the task (simulated by waiting)
     - log the end of the task -}
taskSimulation : (Color, Float, String) -> IO Model String
taskSimulation (color, duration, msg) =
  log color (msg ++ " started")
  |> seq (wait duration msg
          |> IO.andThen (\r -> log color (msg ++ " finished")
                               |> seq (pure r)
                        )
         )

{- The test -}
test : IO Model Msg
test = IO.batch [ -- We batch 5 tasks with different duration
          ("red"     , 4000.0, "A"),
          ("green"   , 2000.0, "B"),
          ("blue"    , 5000.0, "C"),
          ("brown"   , 3000.0, "D"),
          ("magenta" , 1000.0, "E")
       ] |> IO.andThen taskSimulation -- Run each task
         |> IO.andThen (\r -> log "black" ("Returns: " ++ r)) -- log the result

main : IO.Program () Model Msg 
main =
  IO.sandbox {
    init = \_ -> ([], test),
    view = view,
    subscriptions = IO.dummySub
  }