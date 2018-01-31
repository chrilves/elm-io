# Elm-IO

[Documentation](http://package.elm-lang.org/packages/chrilves/elm-io/latest)

> Some [rules] can be bent. Others can be broken. Understand?
> *-- Morpheus*

This projects provides **pure Elm** tools whose aim is to make programming in
Elm more **composable** even if it means bending a bit the rules of [The Elm
Architecture](https://guide.elm-lang.org/architecture/).

A classic Elm application that follows [The Elm Architecture](https://guide.elm-lang.org/architecture/) is structured like this:
- a `model` data type representing the model at any given time.
- a `msg` data type representing events either fired by the view or the runtime (mouse click, input change, http response, ...) called messages.
- the `update: msg -> model -> (model, Cmd msg)` function is called on every message received to compute the new value of the model. It can output [commands](http://package.elm-lang.org/packages/elm-lang/core/latest/Platform-Cmd#Cmd) which are tasks handled by the runtime. On completion, commands return a message which trigger once again the `update` function.
- the `view: model -> Html msg` renders the current value of the model into HTML content. This HTML can define messages to send on events.

This approach has some limitations:
- The callback passed to `onInput : (String -> msg) -> Attribute msg` and many other event handlers can decide which message to send based on the input string, but is forced to send one.
- if you want to execute a command in response to an event happening in the view, the view has to trigger a message that will be interpreted by the update function which will output the command ...
- the command type ([Cmd](http://package.elm-lang.org/packages/elm-lang/core/latest/Platform-Cmd#Cmd)) is not a monad. It means commands do not compose! For example chaining commands has to be handled in the update function or by using another type such as [tasks](http://package.elm-lang.org/packages/elm-lang/core/latest/Task).

All of this makes perfect sense from an architectural point of view. [The Elm Architecture](https://guide.elm-lang.org/architecture/) has many benefits like isolation of rendering, state and effects. This project is for those ready to trade these benefits for more *flexibility* and *conciseness*. If your update function is littered with command scheduling code or/and your message type looks more like boilerplate than business, then this package is made for you! 

You have two options:
- the [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM) approach lets you program the way you used to but lets you trigger commands in the view and chain commands as you like. The model is still updated in the update function, not in the view!
- the [IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO) approach, in addition of the [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM)'s benefits, lets you read and write the state directly in commands. You can then alter the state directly from the view.

## The [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM) monad

The [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM)
monad is the command type (*Cmd*) turned into a monad.
It enables to chain effects easily using classic monadic operations without
having to encode complex scheduling in the update function.

## The [IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO) monad

The [IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO) monad
is like the [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM) monad
enriched with state altering effect. Thus command effects and model modifications can be
mixed easily. Furthermore the view and subscriptions can not only emit messages but also
[IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO)s.

### Example

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

## Examples from http://elm-lang.org/examples translated into [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM) and [IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO)

The [examples](https://github.com/chrilves/elm-io/tree/master/examples) folder contains examples from http://elm-lang.org/examples converted into [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM)
and [IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO) ways. Please read the [README.md](https://github.com/chrilves/elm-io/tree/master/examples/README.md) file in this folder for more details on examples.

## Need help?

If you have questions and/or remarks, contact me on twitter at [@chrilves](https://twitter.com/chrilves)