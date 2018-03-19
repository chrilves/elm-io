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

## Installing version 2.0.0 beta

Version 2.0.0 is still in beta but use it thanks to [elm-github-install](https://github.com/gdotdesign/elm-github-install). Adapt your `elm-package.json` with:

```json
{
    "dependencies": {
        "chrilves/elm-io": "2.0.0 <= v < 3.0.0"
    },
    "dependency-sources": {
        "chrilves/elm-io": "git@github.com:chrilves/elm-io"
    }
}
```

Then run `elm-install`.

## The [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM) monad

The [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM)
monad is the command type ([Cmd](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Cmd#Cmd)) turned into a monad.
It enables to chain effects easily using classic monadic operations without
having to encode complex scheduling in the update function.

A program using [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM)
is generally built arround
[CmdM.program](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM#program),
[CmdM.vDomProgram](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM#vDomProgram),
[CmdM.programWithFlags](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM#programWithFlags)
or [CmdM.vDomProgramWithFlags](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM#vDomProgramWithFlags)
depending on if this is a headless program or if flags are required. For more specific needs,
you can use [CmdM.transform](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM#transform)
and [CmdM.transformWithFlags](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM#transformWithFlags).

[CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM) is used very much like
[Cmd](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Cmd#Cmd). The main difference
is the view outputs `Html (CmdM Msg)` instead of `Html Msg`. You're not forced to refactor your view
to use [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM):

```elm
classicTeaView: Model -> Html Msg

cmdmView: Model -> Html (CmdM Msg)
cmdmView model = classicTeaView |> Html.map CmdM.pure
```

The general way of using [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM) is
lifting a `Cmd a` value into a `CmdM a` one by [CmdM.lift](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM#lift)
and chain them by [CmdM.andThen](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM#andThen) or [CmdM.ap](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM#ap). The module [CmdM.Infix](package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM-Infix) provides infix notation for these operators.


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

Like [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM), a program using
[IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO) is generally built arround one of the
many *IO.\*Program\** functions. These function cover web and headless programs, run with or without
flags. In addition the functions named *beginner\** offer a simple and conside way to run most
[IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO) programs. For more specific needs,
you can use [IO.transform](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO#transform)
and [IO.transformWithFlags](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO#transformWithFlags).

With [IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO), reading and writing the model
is done with  [IO.get](http://package.elm-lang.org/packages/chrilves/elm-io/1.2.1/IO#get),
[IO.set](http://package.elm-lang.org/packages/chrilves/elm-io/1.2.1/IO#set) and
[IO.modify](http://package.elm-lang.org/packages/chrilves/elm-io/1.2.1/IO#modify).
It means this kind of code becomes possible:

```elm
action : IO Model Msg
action =
  IO.get |> IO.andThen (\model -> -- First we read the model
    let
      -- The classic Http command
      httpCommand : Cmd (Result Error Model)
      httpCommand = Http.send identity (Http.get "https://example.com/my/api/action" decoder)
    in
      -- First we lift the Cmd command into IO  
      -- then compose it by andThen with a function to deal with the response
      IO.lift httpCommand |> IO.andThen (\response ->
        case response of
          Ok newModel -> IO.set newModel -- and set the new model on success
          Err _       -> IO.none         -- or do nothing on failure
  ))
```

Requiring all [IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO) actions to work
on the whole model would break composability, which would be petty bad obviously. Fortunately
[IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO) play well with
[optics](https://github.com/arturopala/elm-monocle):

```elm
import Monocle.Lens exposing (..)

-- An IO action whose model is an integer
actionOnInt : IO Int ()
actionOnInt = IO.modify (\x -> x + 1)

type alias Model = { number : Int, name : String }

lensFromIntToModel : Lens Model Int
lensFromIntToModel =
  { get = \model -> model.number,
    set = \i model -> { model | number = i }
  }

-- an IO action whose model is a Model
actionOnModel : IO Model ()
actionOnModel = IO.lens lensFromIntToModel actionOnInt
```

To avoid having to use optics when not needed, it is advised to use [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM)
for model agnostic actions and lift [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM) to
[IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO) at the last moment by
[IO.liftM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO#liftM).

## Examples from http://elm-lang.org/examples translated into [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM) and [IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO)

The [examples](https://github.com/chrilves/elm-io/tree/master/examples) folder contains examples from http://elm-lang.org/examples converted into [CmdM](http://package.elm-lang.org/packages/chrilves/elm-io/latest/CmdM)
and [IO](http://package.elm-lang.org/packages/chrilves/elm-io/latest/IO) ways. Please read the [README.md](https://github.com/chrilves/elm-io/tree/master/examples/README.md) file in this folder for more details on examples.

## Need help?

If you have questions and/or remarks, contact me on twitter at [@chrilves](https://twitter.com/chrilves)