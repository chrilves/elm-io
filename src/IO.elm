module IO exposing
    ( IO
    , Program, sandbox, element, document, application
    , pure, lift, liftM, liftUpdate
    , get, set, modify
    , map, andThen, join, ap, seq, traverse, mapM
    , lens, optional, iso, prism
    , none, dummyUpdate, dummySub
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

@docs map, andThen, join, ap, seq, traverse, mapM


# Passing from a model to another via [optics](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest)

@docs lens, optional, iso, prism


# Dummy values

@docs none, dummyUpdate, dummySub


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
import CmdM.Internal as CmdMI
import Html exposing (..)
import IO.Internal exposing (..)
import Monocle.Iso exposing (..)
import Monocle.Lens exposing (..)
import Monocle.Optional exposing (..)
import Monocle.Prism exposing (..)
import Platform.Cmd exposing (..)
import Url exposing (..)


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
batch l = Impure (\model -> (model, CmdMI.Batch (List.map Pure l)))

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
                Impure m -> Impure (baseMap aux m)
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
                Impure x -> Impure (baseMap aux x)
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


{-| Run the first argument, ignore the result, then run the second.
-}
seq : IO model a -> IO model b -> IO model b
seq = map (\_ -> identity) >> ap

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

{-| Lift a _Cmd_ as an [IO](#IO).
-}
lift : Cmd a -> IO model a
lift cmd = Impure (\s -> (s, CmdMI.Command (Cmd.map Pure cmd)))


{-| Lift a [CmdM](../CmdM) into an [IO](#IO)
-}
liftM : CmdM a -> IO model a
liftM cmdm =
    case cmdm of
        CmdMI.Pure a -> Pure a
        CmdMI.Impure b -> Impure (\s -> (s, CmdMI.baseMap liftM b))


{-| Lift a classic update function into an [IO](#IO).
-}
liftUpdate : (model -> ( model, Cmd a )) -> IO model a
liftUpdate f = Impure (\m -> let (m2, cmd) = f m
                             in (m2, CmdMI.Command (Cmd.map Pure cmd))
                      )

-- State


{-| An [IO](#IO) that returns the current model.
-}
get : IO model model
get = Impure (\s -> (s, CmdMI.Batch [Pure s]))


{-| An [IO](#IO) that sets the model.
-}
set : model -> IO model ()
set s = Impure (\_ -> (s, CmdMI.Batch [Pure ()]))


{-| A [IO](#IO) that modify the model.
-}
modify : (model -> model) -> IO model ()
modify f = Impure (\s -> (f s, CmdMI.Batch [Pure ()]))

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
    let aux iob =
            case iob of
                Pure msg ->
                    Pure msg

                Impure base -> Impure (\b ->
                    let (a, cmdmibase) = base (ll.get b)
                    in (ll.set a b, CmdMI.baseMap aux cmdmibase))
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

                Impure base -> Impure (\b ->
                    case opt.getOption b of
                        Nothing -> (b, CmdMI.Batch [])
                        Just a  ->
                            let (a2, cmdmibase) = base a
                            in (opt.set a2 b, CmdMI.baseMap aux cmdmibase))
    in aux


{-| Congruence by a [Iso](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Iso) on an [IO](#IO).
Just like lenses but with [Iso](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Iso).
-}
iso : Iso b a -> IO a msg -> IO b msg
iso liso =
    let aux iob =
            case iob of
                Pure msg -> Pure msg
                Impure x -> Impure (\b ->
                    let (a, cmdmibase) = x (liso.get b)
                    in (liso.reverseGet a, CmdMI.baseMap aux cmdmibase))
    in aux


{-| Congruence by a [Prism](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Prism) on an [IO](#IO).
Just like lenses but with [Prism](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Iso).
If the prism returns `Nothing`, then the [IO](#IO) does nothing.
-}
prism : Prism b a -> IO a msg -> IO b msg
prism { getOption, reverseGet } =
    let aux ioa =
            case ioa of
                Pure msg -> Pure msg

                Impure base -> Impure (\b ->
                    case getOption b of
                        Nothing -> (b, CmdMI.Batch [])

                        Just a -> let (a2, cmdmibase) = base a
                                  in (reverseGet a2, CmdMI.baseMap aux cmdmibase))
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
                Pure msg    -> recur (f msg) model
                Impure base ->
                    let (model2, cmdmibase) = base model
                    in  case cmdmibase of
                            CmdMI.Batch l     -> let (modelEnd, iosEnd) = List.foldl (\io (modelAcc, ios) ->
                                                                                        let (modelRet, cmd) = recur io modelAcc
                                                                                        in (modelRet, cmd :: ios)
                                                                                     ) (model2, []) l
                                                 in (modelEnd, Cmd.batch iosEnd) 
                            CmdMI.Command cmd -> (model2, cmd)
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
    let
        newUpdate =
            runUpdate update
    in
    { update = newUpdate
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
    let
        new =
            transform args.update
    in
    Browser.element
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
    let
        new =
            transform args.update
    in
    Browser.document
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
    let
        new =
            transform args.update
    in
    Browser.application
        { update = new.update
        , init = \f u k -> new.initTransformer (args.init f u k)
        , view = args.view
        , subscriptions = args.subscriptions
        , onUrlRequest = args.onUrlRequest
        , onUrlChange = args.onUrlChange
        }
