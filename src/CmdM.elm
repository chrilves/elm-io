module CmdM exposing
    ( CmdM
    , Program, element, document, application
    , pure, lift, none
    , map, andThen, join, ap, flap, compose, seq, traverse, mapM
    , transform
    , batch, batchM
    )


{-| This module provides a monadic interface for commands.

Basically [CmdM](#CmdM) is like `Cmd` but is a monad, which means
you can chain effects as you like!

@docs CmdM


# Runing an Elm application with [CmdM](#CmdM)

This module port the four main way of running an Elm application to [CmdM](#CmdM).

@docs Program, element, document, application


# Lifting values and commands into [CmdM](#CmdM)

@docs pure, lift, none


# Classic monadic operations

@docs map, andThen, join, ap, flap, compose, seq, traverse, mapM

# Transform CmdM into regular Elm

@docs transform


# Batch operations

Beware that batch operations might not do what you think. The execution order of
messages and commands is **not defined**.

@docs batch, batchM

-}

import Browser exposing (..)
import Browser.Navigation exposing (Key)
import CmdM.Internal exposing (..)
import Html exposing (Html)
import Platform.Cmd exposing (..)
import Url exposing (..)

{-| Monadic interface for commands.

A value of type `CmdM msg` is an effectful
computation that can perform commands and
contains values of type `msg`.

-}
type alias CmdM msg =
    CmdM.Internal.CmdM msg

-- Monadic

{-| Returns a [CmdM](#CmdM) whose only effect is containing the value given to [pure](#pure).
-}
pure : a -> CmdM a
pure a = Pure a

{-| Send messages in batch
-}
batch : List a -> CmdM a
batch l = Impure (Batch (List.map Pure l))


{-| Transforms an Elm command into a monadic command [CmdM](#CmdM).
-}
lift : Cmd a -> CmdM a
lift cmd = Impure (Command (Cmd.map Pure cmd))


{-| Map a function over an [CmdM](#CmdM).

**Laws**

  - `map (f >> g) = (map f) >> (map g)`
  - `map identity = identity`

-}
map : (a -> b) -> CmdM a -> CmdM b
map f =
    let aux cmdm =
            case cmdm of
                Pure a   -> Pure (f a)
                Impure m -> Impure (effectMap aux m)
    in aux


{-| Chains [CmdM](#CmdM)s.

If you have a `CmdM a` and a function which given
a `a` can give you a `CmdM b` depending on the value
of type `a` given to the function. Then [andThen](#andThen) gives you
a `CmdM b` that will run the first [CmdM](#CmdM) and then apply
the function.

**Laws**

  - `andThen pure = identity`
  - `andThen (f >> pure) = map f`
  - `(andThen f) >> (andThen g) = andThen (a -> andThen g (f a))`

-}
andThen : (a -> CmdM b) -> CmdM a -> CmdM b
andThen f =
    let aux m =
            case m of
                Pure a   -> f a
                Impure x -> Impure (effectMap aux x)
    in aux


{-| Flatten a [CmdM](#CmdM) containing a [CmdM](#CmdM) into a simple [CmdM](#CmdM).

**Laws**

  - `join (pure m) = m`

-}
join : CmdM (CmdM a) -> CmdM a
join = andThen identity


{-| Transform a [CmdM](#CmdM) containing functions into functions on [CmdM](#CmdM).
It enable to easily lift functions to [CmdM](#CmdM).

**Laws**

  - `ap (pure identity) = identity`
  - `ap (pure (f >> g)) = ap (pure f) >> ap (pure g)`

-}
ap : CmdM (a -> b) -> CmdM a -> CmdM b
ap mf ma = mf |> andThen (\f -> map f ma)

{-| Flipped version of ap. To be used like:

    pure f |> flap arg1 |> flap arg2 ...
-}
flap : CmdM a -> CmdM (a -> b) -> CmdM b
flap ma mf = ap mf ma

{-| Composition of monadic functions
-}
compose : (b -> CmdM c) -> (a -> CmdM b) -> a -> CmdM c
compose g f a = f a |> andThen g

{-| Run the second argument, ignore the result, then run the first one.
    To be used in

    first |> seq second
-}
seq : CmdM b -> CmdM a -> CmdM b
seq second first = first |> andThen (\_ -> second)

-- Monoid

{-| A [CmdM](#CmdM) doing nothing (an containing no values!).
-}
none : CmdM a
none = batch []


{-| **I strongly discourage you from using it. Use [mapM](#mapM) instead.**
Group commands in a batch. Its behavior may not be what you expect!
-}
batchM : List (CmdM a) -> CmdM a
batchM l = join (batch l)

{-| You can think of traverse like a [map](#map) but with effects.
It maps a function performing [CmdM](#CmdM) effects over a list.
-}
traverse : (a -> CmdM b) -> List a -> CmdM (List b)
traverse f l =
    case l of
        []       -> pure []
        hd :: tl -> ap (ap (pure (::)) (f hd)) (traverse f tl)

{-| Transform a list of [CmdM](#CmdM) into an [CmdM](#CmdM) of list.
-}
mapM : List (CmdM a) -> CmdM (List a)
mapM = traverse identity

-- Platform

{-| Program using [CmdM](#CmdM).
-}
type alias Program flags model msg =
    Platform.Program flags model (CmdM msg)

-- The core of all the [CmdM](#CmdM) monad! It runs the [CmdM](#CmdM) monad using the update function.

runUpdate : (msg -> model -> ( model, CmdM msg )) -> CmdM msg -> model -> ( model, Cmd (CmdM msg) )
runUpdate f =
    let
        aux : CmdM msg -> model -> ( model, Cmd (CmdM msg) )
        aux cmdm model =
            case cmdm of
                Pure msg -> let (model2, cmdm2) = f msg model
                            in aux cmdm2 model2
                Impure (Command cmd) -> (model, cmd)
                Impure (Batch l) ->
                    let (modelEnd, cmdsEnd) =
                            List.foldl (\cmdm2 (modelAcc, cmdsAcc) ->
                                            let (m3, cs3) = aux cmdm2 modelAcc
                                            in (m3, cs3 :: cmdsAcc)
                                       ) (model, []) l
                    in (modelEnd, Cmd.batch cmdsEnd)
    in aux


{-| Transform a program using [CmdM](#CmdM) into a normal program.
-}
transform :
    (msg -> model -> ( model, CmdM msg ))
    ->
        { update : CmdM msg -> model -> ( model, Cmd (CmdM msg) )
        , initTransformer : ( model, CmdM msg ) -> ( model, Cmd (CmdM msg) )
        }
transform update =
    let
        newUpdate =
            runUpdate update
    in
    { update = newUpdate
    , initTransformer = \( m, cmdm ) -> newUpdate cmdm m
    }


{-| Transform an element program using [CmdM](#CmdM) into a normal element program.
-}
element :
    { init : flags -> ( model, CmdM msg )
    , view : model -> Html (CmdM msg)
    , update : msg -> model -> ( model, CmdM msg )
    , subscriptions : model -> Sub (CmdM msg)
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


{-| Transform a document program using [CmdM](#CmdM) into a normal document program.
-}
document :
    { init : flags -> ( model, CmdM msg )
    , view : model -> Document (CmdM msg)
    , update : msg -> model -> ( model, CmdM msg )
    , subscriptions : model -> Sub (CmdM msg)
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


{-| Transform an application program using [CmdM](#CmdM) into a normal application program.
-}
application :
    { init : flags -> Url -> Key -> ( model, CmdM msg )
    , view : model -> Document (CmdM msg)
    , update : msg -> model -> ( model, CmdM msg )
    , subscriptions : model -> Sub (CmdM msg)
    , onUrlRequest : UrlRequest -> CmdM msg
    , onUrlChange : Url -> CmdM msg
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
