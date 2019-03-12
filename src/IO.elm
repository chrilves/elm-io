module IO exposing
    ( IO
    , Program, sandbox, element, document, application
    , pure, lift, liftM, liftUpdate
    , get, set, modify
    , map, andThen, join, ap, flap, compose, seq, traverse, mapM
    , lens, optional, iso, prism, replace
    , none, dummyUpdate, dummySub
    , yield, forceRendering
    , transform
    , batch, batchM
    )

{-| This module provides a monadic interface for _The Elm Architecture_.

Basically [IO](#IO) is a monad enabing two kinds of effects :

  - **model modification**:
    it is a [state monad](http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/)
    whose state is the model. It can be read by [get](#get) and written by [set](#set).
  - _Cmd_ and [CmdM](../CmdM) **commands**.

@docs IO


# Runing a web application with [IO](#IO)

This module port the two main ways of running an Elm application to [IO](#IO).

@docs Program, sandbox, element, document, application


# Lifting values and commands into [IO](#IO)

@docs pure, lift, liftM, liftUpdate


# The model as a state

@docs get, set, modify


# Classic monadic operations

@docs map, andThen, join, ap, flap, compose, seq, traverse, mapM

# Passing from a model to another via [optics](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest)

@docs lens, optional, iso, prism, replace

# Dummy values

@docs none, dummyUpdate, dummySub

# Forces Elm rendering

@docs yield, forceRendering

# Transform IO into regular Elm

@docs transform


# Batch operations

Beware that batch operations might not do what you think. The execution order of
messages and commands is **not defined**.

@docs batch, batchM

-}

import Browser exposing (..)
import Browser.Navigation exposing (Key)
import CmdM exposing (..)
import CmdM.Internal
import Html exposing (..)
import IO.Internal exposing (..)
import Monocle.Iso exposing (..)
import Monocle.Lens exposing (..)
import Monocle.Optional exposing (..)
import Monocle.Prism exposing (..)
import Platform.Cmd exposing (..)
import Url exposing (..)
import Process
import Task


{-| Monadic interface for _The Elm Architecture_.

A value of type `IO model a` is an effectful
computation that can modify the model `model`,
perform commands and contains values of type `a`.

-}
type alias IO model msg =
    IO.Internal.IO model msg

-- Monadic


{-| Returns an [IO](#IO) whose only effect is containing the value given to [pure](#pure).
-}
pure : a -> IO model a
pure a =
    Pure a


{-| Send messages in batch mode
-}
batch : List a -> IO model a
batch l = Impure (Batch (List.map Pure l))

{-| Lift a _Cmd_ as an [IO](#IO).
-}
lift : Cmd a -> IO model a
lift cmd = Impure (Command (Cmd.map Pure cmd))

{-| An [IO](#IO) that returns the current model.
-}
get : IO model model
get = Impure (Get Pure)


{-| An [IO](#IO) that sets the model.
-}
set : model -> IO model ()
set s = Impure (Set s Pure)


{-| Map a function over an [IO](#IO).

**Laws**

  - `map (f >> g) = (map f) >> (map g)`
  - `map identity = identity`

-}
map : (a -> b) -> IO model a -> IO model b
map f =
    let aux ioa =
            case ioa of
                Pure a   -> Pure (f a)
                Impure m -> Impure (effectMap aux m)
    in aux


{-| Chains [IO](#IO)s.

If you have an _IO model a_ and a function which given
a _a_ can give you an _IO model b_ depending on the value
of type _a_ given to the function. Then [andThen](#andThen) gives you
an _IO model b_ that will run the first [IO](#IO) and then apply
the function.

**Laws**

  - `andThen pure = identity`
  - `andThen (f >> pure) = map f`
  - `(andThen f) >> (andThen g) = andThen (a -> andThen g (f a))`

-}
andThen : (a -> IO model b) -> IO model a -> IO model b
andThen f =
    let aux m =
            case m of
                Pure a   -> f a
                Impure x -> Impure (effectMap aux x)
    in aux


{-| Flatten an [IO](#IO) containing an [IO](#IO) into a simple [IO](#IO).

**Laws**

  - `join (pure m) = m`

-}
join : IO model (IO model a) -> IO model a
join = andThen identity


{-| Transform an [IO](#IO) containing functions into functions on [IO](#IO)
It enable to easily lift functions to [IO](#IO).

**Laws**

  - `ap (pure identity) = identity`
  - `ap (pure (f >> g)) = ap (pure f) >> ap (pure g)`

-}
ap : IO model (a -> b) -> IO model a -> IO model b
ap mf ma = andThen (\y -> map y ma) mf

{-| Flipped version of ap. To be used like:

    pure f |> flap arg1 |> flap arg2 ...
-}
flap : IO model a -> IO model (a -> b) -> IO model b
flap ma mf = ap mf ma

{-| Composition of monadic functions
-}
compose : (b -> IO m c) -> (a -> IO m b) -> a -> IO m c
compose g f a = f a |> andThen g

{-| Run the second argument, ignore the result, then run the first one.
    To be used in

    first |> seq second
-}
seq : IO model b -> IO model a -> IO model b
seq second first = first |> andThen (\_ -> second)

-- Monoid

{-| An [IO](#IO) doing nothing (an containing no values!).
-}
none : IO model a
none = batch []


{-| **Its use is strongly discouraged! Use [mapM](#mapM) instead!**
Combine a list of [IO](#IO).
-}
batchM : List (IO model a) -> IO model a
batchM l = join (batch l)

-- Tansformer

{-| Lift a [CmdM](../CmdM) into an [IO](#IO)
-}
liftM : CmdM a -> IO model a
liftM cmdm =
    case cmdm of
        CmdM.Internal.Pure a   -> Pure a
        CmdM.Internal.Impure imp -> Impure (
                case imp of
                    CmdM.Internal.Batch l   -> Batch (List.map liftM l)
                    CmdM.Internal.Command c -> Command (Cmd.map liftM c)
            )


{-| Lift a classic update function into an [IO](#IO).
-}
liftUpdate : (model -> ( model, Cmd a )) -> IO model a
liftUpdate f = Impure (Get (\m0 -> Impure (
                let (m2, cmd) = f m0
                in Set m2 (\() -> Impure (Command (Cmd.map Pure cmd)))
               )))
-- State

{-| A [IO](#IO) that modify the model.
-}
modify : (model -> model) -> IO model ()
modify f = Impure (Get (\m -> Impure (Set (f m) Pure)))

-- Optics

{-| Congruence by a [Lens](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Lens) on an [IO](#IO).

It would be silly to force users to redefine every [IO](#IO)
for each application model. Lenses enable to lift an [IO](#IO)
action on a model _a_ to the same [IO](#IO) but action on a
model _b_.

You can then define your [IO](#IO) on the minimal model and
lift them to you real application's model when needed.

-}
lens : Lens b a -> IO a msg -> IO b msg
lens ll =
    let aux ioa =
            case ioa of
                Pure msg -> Pure msg
                Impure x -> Impure (
                  case x of
                    Get f   -> Get (ll.get >> f >> aux)
                    Set a f -> Get (\b -> Impure (Set (ll.set a b) (f >> aux)))
                    Batch l -> Batch (List.map aux l)
                    Command c -> Command (Cmd.map aux c)
                  )
    in aux

{-| Congruence by a [Optional](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Optional) on an [IO](#IO).
Just like lenses but with [Optional](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Optional).
If the optional returns `Nothing`, then the [IO](#IO) does nothing.
-}
optional : Optional b a -> IO a msg -> IO b msg
optional opt =
    let aux ioa =
            case ioa of
                Pure msg -> Pure msg
                Impure x -> Impure (
                  case x of
                    Get f   -> Get (\b -> case opt.getOption b of
                                            Nothing -> none
                                            Just a  -> aux (f a)
                                   )
                    Set a f -> Get (\b -> Impure (Set (opt.set a b) (f >> aux)))
                    Batch l -> Batch (List.map aux l)
                    Command c -> Command (Cmd.map aux c)
                  )
    in aux


{-| Congruence by a [Iso](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Iso) on an [IO](#IO).
Just like lenses but with [Iso](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Iso).
-}
iso : Iso b a -> IO a msg -> IO b msg
iso liso =
    let aux iob =
            case iob of
                Pure msg -> Pure msg
                Impure x -> Impure (
                    case x of
                        Get f   -> Get (liso.get >> f >> aux)
                        Set a f -> Set (liso.reverseGet a) (f >> aux)
                        Batch l -> Batch (List.map aux l)
                        Command c -> Command (Cmd.map aux c)
                  )
    in aux


{-| Congruence by a [Prism](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Prism) on an [IO](#IO).
Just like lenses but with [Prism](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Iso).
If the prism returns `Nothing`, then the [IO](#IO) does nothing.
-}
prism : Prism b a -> IO a msg -> IO b msg
prism prsm =
    let aux ioa =
            case ioa of
                Pure msg -> Pure msg
                Impure x -> Impure (
                  case x of
                    Get f   -> Get (\b -> case prsm.getOption b of
                                            Nothing -> none
                                            Just a  -> aux (f a)
                                   )
                    Set a f -> Set (prsm.reverseGet a) (f >> aux)
                    Batch l -> Batch (List.map aux l)
                    Command c -> Command (Cmd.map aux c)
                  )
    in aux


{-| Replace get and set by custom functions -}
replace : IO b a -> (a -> IO b ()) -> IO a x -> IO b x
replace rget rset =
  let aux : IO a x -> IO b x
      aux ioa =
        case ioa of
          Pure x -> Pure x
          Impure (Get k)   -> rget |> andThen (k >> aux)
          Impure (Set a k) -> rset a |> andThen (k >> aux)
          Impure (Batch l) -> Impure (Batch (List.map aux l))
          Impure (Command c) -> Impure (Command (Cmd.map aux c))
  in aux              

{-| You can think of traverse like a [map](#map) but with effects.
It maps a function performing [IO](#IO) effects over a list.
-}
traverse : (a -> IO model b) -> List a -> IO model (List b)
traverse f =
    let aux l =
            case l of
                [] -> pure []
                hd :: tl -> ap (ap (pure (::)) (f hd)) (aux tl)
    in aux

{-| Transform a list of [IO](#IO) into an [IO](#IO) of list.
-}
mapM : List (IO model a) -> IO model (List a)
mapM = traverse identity

-- Dummy

{-| Dummy update function.
-}
dummyUpdate : a -> IO b c
dummyUpdate _ = none


{-| Dummy subscription function
-}
dummySub : a -> Sub b
dummySub a = Sub.none

{-| Identity function that forces Elm to render
    the current state. Is equivalent to sleep for
    0 milliseconds.
-}
yield : msg -> IO model msg
yield msg = lift (Task.perform (\_ -> msg) (Process.sleep 0))

{-| Forces Elm to render every set operation (model update).
    This is MUCH SLOWER than normal set operations.
-}
forceRendering : IO a b -> IO a b
forceRendering = replace get (set |> compose yield)

-- Platform

{-| Program using [IO](#IO).
-}
type alias Program flags model msg =
    Platform.Program flags model (IO model msg)

-- The core of all the [IO](#IO) monad! It runs the [IO](#IO) monad using the update function.

runUpdate : (msg -> IO model msg) -> IO model msg -> model -> ( model, Cmd (IO model msg) )
runUpdate f =
  let recur : IO model msg -> model -> (model, Cmd (IO model msg))
      recur io1 model =
        case io1 of
          Pure msg -> recur (f msg) model
          Impure x ->
            case x of
              Get k   -> recur (k model) model
              Set m k -> recur (k ())    m
              Batch l -> let (m4, ios4) = List.foldl (\io (ma, ios) ->
                                                        let (m3, cmd) = recur io ma
                                                        in (m3, cmd :: ios)
                                                     ) (model, []) l
                         in (m4, Cmd.batch ios4)
              Command cmd -> (model, cmd)
  in recur


{-| Transform a program using [IO](#IO) into a normal program.
-}
transform :
    (msg -> IO model msg)
    ->
        { update : IO model msg -> model -> ( model, Cmd (IO model msg) )
        , initTransformer : ( model, IO model msg ) -> ( model, Cmd (IO model msg) )
        }
transform update =
  let newUpdate = runUpdate update
  in { update = newUpdate
     , initTransformer = \( m, io ) -> newUpdate io m
     }

{-| Transform an element program using [IO](#IO) into a normal element program.
-}
element :
    { init : flags -> ( model, IO model msg )
    , view : model -> Html (IO model msg)
    , update : msg -> IO model msg
    , subscriptions : model -> Sub (IO model msg)
    }
    -> Program flags model msg
element args =
  let new = transform args.update
  in Browser.element
       { update = new.update
       , init = args.init >> new.initTransformer
       , view = args.view
       , subscriptions = args.subscriptions
       }

{-| Transform a sandbox program using [IO](#IO) into a normal sandbox program.
-}
sandbox :
    { init : flags -> ( model, IO model msg )
    , view : model -> Html (IO model msg)
    , subscriptions : model -> Sub (IO model msg)
    }
    -> Program flags model msg
sandbox args = element { init = args.init,
                         view = args.view,
                         update = dummyUpdate,
                         subscriptions = args.subscriptions
                       }

{-| Transform a document program using [IO](#IO) into a normal document program.
-}
document :
    { init : flags -> ( model, IO model msg )
    , view : model -> Document (IO model msg)
    , update : msg -> IO model msg
    , subscriptions : model -> Sub (IO model msg)
    }
    -> Program flags model msg
document args =
  let new = transform args.update
  in Browser.document
       { update = new.update
       , init = args.init >> new.initTransformer
       , view = args.view
       , subscriptions = args.subscriptions
       }


{-| Transform an application program using [IO](#IO) into a normal application program.
-}
application :
    { init : flags -> Url -> Key -> ( model, IO model msg )
    , view : model -> Document (IO model msg)
    , update : msg -> IO model msg
    , subscriptions : model -> Sub (IO model msg)
    , onUrlRequest : UrlRequest -> IO model msg
    , onUrlChange : Url -> IO model msg
    }
    -> Program flags model msg
application args =
  let new = transform args.update
  in Browser.application
       { update = new.update
       , init = \f u k -> new.initTransformer (args.init f u k)
       , view = args.view
       , subscriptions = args.subscriptions
       , onUrlRequest = args.onUrlRequest
       , onUrlChange = args.onUrlChange
       }