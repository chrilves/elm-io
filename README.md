# Elm-IO

This projects provide pure Elm tools whose aim is to make programming in
Elm more composable.

Currently it offers two monadic interfaces for Elm commands and
*The Elm Architecture*. I'm completely aware that this goes against
the Elm architecture philosphy but in practice it helps.

## The *CmdM* monad

The *CmdM* monad is the command type (*Cmd*) turned into a monad.
It enables to chain effects easily using classic monadic operations without
having to encode complex scheduling in the update function.

## The *IO* monad

The *IO* monad is *CmdM* augmented with state altering effect. Thus command
effects and model modifictions can be mixed easily. Furthermore the view and
subscritptions can not only emmit messages but also *IO*s.

*IO* is certainly the most terrible offense to the Elm architecture. But
sometimes (often?) it is cleaner and simpler to send as message an *IO*
than polluting the model or encode complex scheduling logic in the message
type while turning the update function into complex interpreter.

## Example

Here is a complete example of a simple page showing a counter

```elm
module Hello exposing (..)

import Html exposing (..)
import Html.Events exposing(..)
import IO exposing (..)

type alias Model  = Int 
type alias Msg = ()

increment : IO Model Msg
increment = IO.modify ((+) 1)

reset : IO Model Msg
reset = IO.set 0

view : Model -> Html (IO Model Msg) 
view m = 
  div [] [
    h1 [] [
      text "Example of an IO program"
    ],
    p [] [
      text ("Counter = " ++ (toString m))
    ],
    button [onClick increment] [
      text "increment"
    ],
    button [onClick reset] [
      text "reset"
    ]
  ]

main : IO.Program Never Model Msg 
main =
  IO.beginnerVDomProgram { init = 0, view = view , subscriptions = IO.dummySub }
```