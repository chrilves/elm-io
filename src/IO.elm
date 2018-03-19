module IO exposing (
  IO,
  {- Monadic      -} pure, map, andThen, join, ap, seq,
  {- Monoid       -} none, batch, combine, list,
  {- Transformer  -} lift, liftM, liftUpdate,
  {- State        -} get, set, modify,
  {- Optics       -} lens, optional, iso, prism,
  {- Traversal    -} traverse, mapM,
  {- Dummy        -} dummyUpdate, dummySub,
  Program,
  {- Run no flags -} transform, beginnerProgram, beginnerVDomProgram, program, vDomProgram,
  {- Run    flags -} transformWithFlags, beginnerProgramWithFlags, beginnerVDomProgramWithFlags, programWithFlags, vDomProgramWithFlags
 )

{-|This module provides a monadic interface for *The Elm Architecture*.

Basically [IO](#IO) is a monad enabing two kinds of effects :
- **model modification**:
    it is a [state monad](http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/)
    whose state is the model. It can be read by [get](#get) and written by [set](#set).
- *Cmd* and [CmdM](../CmdM) **commands**.

@docs IO

# Runing a web application with [IO](#IO)
This module port the two main ways of running an Elm application to [IO](#IO).
@docs Program

## Web applications
@docs beginnerVDomProgram, vDomProgram, beginnerVDomProgramWithFlags, vDomProgramWithFlags

## Headless applications
@docs beginnerProgram, program, beginnerProgramWithFlags, programWithFlags

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
@docs transform, transformWithFlags

# Batch operations
Beware that batch operations might not do what you think. The execution order of
messages and commands is **not defined**.
@docs batch, combine, list
-}

import Platform.Cmd exposing (..)
import Monocle.Lens exposing (..)
import Monocle.Optional exposing (..)
import Monocle.Iso exposing (..)
import Monocle.Prism exposing (..)
import VirtualDom exposing (..)
import CmdM exposing (..)
import CmdM.Internal
import IO.Internal exposing (..)

{-|Monadic interface for *The Elm Architecture*.

A value of type `IO model a` is an effectful
computation that can modify the model `model`,
perform commands and contains values of type `a`.
-}
type alias IO model msg = IO.Internal.IO model msg

-- Monadic

{-|Returns an [IO](#IO) whose only effect is containing the value given to [pure](#pure).-}
pure : a -> IO model a
pure a = Pure a

{-|Send messages in batch mode-}
list : List a -> IO model a
list l =
  case l of
    [x] -> pure x
    _   -> Impure (\model -> (model, List.map Pure l, Cmd.none))

{-|Map a function over an [IO](#IO).

**Laws**
- ```map (f >> g) = (map f) >> (map g)```
- ```map identity = identity```
-}
map : (a -> b) -> IO model a -> IO model b
map f =
  let aux ioa =
        case ioa of
          Pure a   -> Pure (f a)
          Impure m -> Impure (baseMap aux m)
  in aux

{-|Chains [IO](#IO)s.

If you have an *IO model a* and a function which given
a *a* can give you an *IO model b* depending on the value
of type *a* given to the function. Then [andThen](#andThen) gives you
an *IO model b* that will run the first [IO](#IO) and then apply
the function.

**Laws**
- ```andThen pure = identity```
- ```andThen (f >> pure) = map f```
- ```(andThen f) >> (andThen g) = andThen (a -> andThen g (f a))```
-}
andThen : (a -> IO model b) -> IO model a -> IO model b
andThen f =
  let aux m =
        case m of
          Pure a   -> f a
          Impure x -> Impure (baseMap aux x)
  in aux

{-|Flatten an [IO](#IO) containing an [IO](#IO) into a simple [IO](#IO).

**Laws**
- ```join (pure m) = m```
-}
join : IO model (IO model a) -> IO model a
join = andThen identity

{-|Transform an [IO](#IO) containing functions into functions on [IO](#IO)
It enable to easily lift functions to [IO](#IO).

**Laws**
- ```ap (pure identity) = identity```
- ```ap (pure (f >> g)) = ap (pure f) >> ap (pure g)```
-}
ap : IO model (a -> b) -> IO model a -> IO model b
ap mf ma = andThen (flip  map ma) mf

{-|Run the first argument, ignore the result, then run the second.-}
seq : IO model a -> IO model b -> IO model b
seq = map (\_ -> identity) >> ap

-- Monoid

{-|An [IO](#IO) doing nothing (an containing no values!).-}
none : IO model a
none = lift Cmd.none

{-|
**Its use is strongly discouraged! Use [mapM](#mapM) instead!**
Combine a list of [IO](#IO).
-}
batch : List (IO model a) -> IO model a
batch l =
  let 
    accumulate : List (IO model b) -> List (List (IO model b)) -> Cmd (IO model b) -> Base model (IO model b)
    accumulate ios lists cmd =
      case ios of
        []       -> \m -> (m, List.reverse lists |> List.concat, cmd)
        hd :: tl -> case hd of
                      Pure _   -> accumulate tl ([hd] :: lists) cmd
                      Impure f -> \m -> let (m2, list2, cmd2) = f m
                                        in accumulate tl (list2 :: lists) (Cmd.batch [cmd, cmd2]) m2
  in case l of
       []  -> none
       [x] -> x
       _   -> Impure (accumulate l [] Cmd.none)

{-|
**Its use is strongly discouraged! Use [mapM](#mapM) instead!**
Combine two [IO](#IO).
-}
combine : IO model a -> IO model a -> IO model a
combine x y = batch [x,y]

-- Tansformer

{-|Lift a *Cmd* as an  [IO](#IO).-}
lift : Cmd a -> IO model a
lift cmd = Impure (\s -> (s, [], Cmd.map Pure cmd))

{-|Lift a [CmdM](../CmdM) into an [IO](#IO)-}
liftM : CmdM a -> IO model a
liftM cmdm =
  case cmdm of
    CmdM.Internal.Pure a          -> Pure a
    CmdM.Internal.Impure (l, cmd) -> Impure (\s -> (s, List.map liftM l, Cmd.map liftM cmd))

{-|Lift a classic update function into an [IO](#IO).-}
liftUpdate : (model -> (model, Cmd a)) -> IO model a
liftUpdate f = Impure (\m -> let (m2, cmd) = f m
                             in (m2, [], Cmd.map Pure cmd)
                      )

-- State

{-|An [IO](#IO) that returns the current model.-}
get : IO model model
get = Impure (\s -> (s, [Pure s], Cmd.none))

{-|An [IO](#IO) that sets the model.-}
set : model -> IO model ()
set s = Impure (\_ -> (s, [Pure ()], Cmd.none))

{-|A [IO](#IO) that modify the model.-}
modify : (model -> model) -> IO model ()
modify f = Impure (\s -> (f s, [Pure ()], Cmd.none))

-- Optics

{-|Congruence by a [Lens](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Lens) on an [IO](#IO).

It would be silly to force users to redefine every [IO](#IO)
for each application model. Lenses enable to lift an [IO](#IO)
action on a model *a* to the same [IO](#IO) but action on a
model *b*.

You can then define your [IO](#IO) on the minimal model and
lift them to you real application's model when needed.
-}
lens : Lens b a -> IO a msg -> IO b msg
lens {get, set} =
  let aux iob =
        case iob of
          Pure msg -> Pure msg
          Impure x -> Impure (\b -> let (a, list, cmd)   = x (get b)
                                    in (set a b, List.map aux list, Cmd.map aux cmd)
                             )
  in aux

{-|Congruence by a [Optional](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Optional) on an [IO](#IO).
Just like lenses but with [Optional](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Optional).
If the optional returns `Nothing`, then the [IO](#IO) does nothing.
-}
optional : Optional b a -> IO a msg -> IO b msg
optional {getOption, set} =
  let aux ioa =
        case ioa of
          Pure msg -> Pure msg
          Impure x -> Impure (\b -> case getOption b of
                                      Nothing -> (b, [], Cmd.none)
                                      Just a  -> let (a2, list, ioa) = x a
                                                 in (set a2 b, List.map aux list, Cmd.map aux ioa)
                            )
  in aux

{-|Congruence by a [Iso](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Iso) on an [IO](#IO).
Just like lenses but with [Iso](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Iso).
-}
iso : Iso b a -> IO a msg -> IO b msg
iso {get, reverseGet} =
  let aux iob =
        case iob of
          Pure msg -> Pure msg
          Impure x -> Impure (\b -> let (a, list, cmd)   = x (get b)
                                    in (reverseGet a, List.map aux list, Cmd.map aux cmd)
                             )
  in aux

{-|Congruence by a [Prism](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Prism) on an [IO](#IO).
Just like lenses but with [Prism](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Iso).
If the prism returns `Nothing`, then the [IO](#IO) does nothing.
-}
prism : Prism b a -> IO a msg -> IO b msg
prism {getOption, reverseGet} =
  let aux ioa =
        case ioa of
          Pure msg -> Pure msg
          Impure x -> Impure (\b -> case getOption b of
                                      Nothing -> (b, [], Cmd.none)
                                      Just a  -> let (a2, list, ioa) = x a
                                                 in (reverseGet a2, List.map aux list, Cmd.map aux ioa)
                            )
  in aux


{-|You can think of traverse like a [map](#map) but with effects.
It maps a function performing [IO](#IO) effects over a list.
-}
traverse : (a -> IO model b) -> List a -> IO model (List b)
traverse f =
 let aux l =
      case l of
        []       -> pure []
        hd :: tl -> ap (ap (pure (::)) (f hd)) (aux tl)
  in aux

{-|Transform a list of [IO](#IO) into an [IO](#IO) of list.-}
mapM : List (IO model a) -> IO model (List a)
mapM = traverse identity

-- Dummy

{-|Dummy update function.-}
dummyUpdate : a -> IO b c
dummyUpdate m = none

{-|Dummy subscription function-}
dummySub : a -> Sub b
dummySub a = Sub.none

-- Platform

{-|Program using [IO](#IO).-}
type alias Program flags model msg = Platform.Program flags model (IO model msg)

-- The core of all the [IO](#IO) monad! It runs the [IO](#IO) monad using the update function.
runUpdate : (msg -> IO model msg) -> IO model msg -> model -> (model, Cmd (IO model msg))
runUpdate f io =
  let recur : List (IO model msg) -> Cmd (IO model msg) -> model -> (model, Cmd (IO model msg))
      recur acc cmd =
        case acc of
          []       -> \model -> (model, cmd)
          hd :: tl -> case hd of
                        Pure msg -> recur ((f msg) :: tl) cmd
                        Impure f -> \model -> let (model2, list, cmd2) = f model
                                              in recur (list ++ tl) (Cmd.batch [cmd, cmd2]) model2
  in recur [io] Cmd.none


{-|Transform a program using [IO](#IO) into a normal program.-}
transform :   { y | init : (model, IO model msg), update : msg -> IO model msg }
           -> { y | init : (model, Cmd (IO model msg)), update : IO model msg -> model -> (model , Cmd (IO model msg)) }
transform args =
  let
    update : IO model msg -> model -> (model, Cmd (IO model msg))
    update = runUpdate args.update

    init : (model, Cmd (IO model msg))
    init = let (model0, io0) = args.init
           in update io0 model0

  in { args | init = init, update = update }

{-|Port of *Platform.program* with [IO](#IO) with [dummyUpdate](#dummyUpdate).-}
beginnerProgram : { init  : model,
                    main  : IO model msg,
                    subscriptions : model -> Sub (IO model msg)
                  } -> Program Never model msg
beginnerProgram args = program { init = (args.init, args.main), update = dummyUpdate, subscriptions = args.subscriptions }

{-|Port of *VirtualDom.program* with [IO](#IO) with [dummyUpdate](#dummyUpdate) (also works with *Html*).-}
beginnerVDomProgram : { init  : model,
                        view  : model -> Node (IO model msg),
                        subscriptions : model -> Sub (IO model msg)
                      } -> Program Never model msg
beginnerVDomProgram args = vDomProgram { init = (args.init, none), update = dummyUpdate, subscriptions = args.subscriptions, view = args.view }

{-|Port of *Platform.program* with [IO](#IO).-}
program : { init : (model, IO model msg),
            update : msg -> IO model msg,
            subscriptions : model -> Sub (IO model msg)
          } -> Program Never model msg
program = transform >> Platform.program

{-|Port of *VirtualDom.program* with [IO](#IO) (also works with *Html*).-}
vDomProgram : { init : (model, IO model msg),
                update : msg -> IO model msg,
                subscriptions : model -> Sub (IO model msg),
                view : model -> Node (IO model msg)
              } -> Program Never model msg
vDomProgram = transform >> VirtualDom.program


{-|Transform a program using [IO](#IO) into a normal program.-}
transformWithFlags :   { y | init : flags -> (model, IO model msg), update : msg -> IO model msg }
                    -> { y | init : flags -> (model, Cmd (IO model msg)), update : IO model msg -> model -> (model, Cmd (IO model msg))}
transformWithFlags args =
  let
    update : IO model msg -> model -> (model, Cmd (IO model msg))
    update = runUpdate args.update

    init : flags -> (model, Cmd (IO model msg))
    init flags = let (model0, io0) = args.init flags
                 in update io0 model0

  in { args | init = init, update = update }


{-|Port of *Platform.programWithFlags* with [IO](#IO) with [dummyUpdate](#dummyUpdate).-}
beginnerProgramWithFlags : { init  : flags -> model,
                             main  : IO model msg,
                             subscriptions : model -> Sub (IO model msg)
                           } -> Program flags model msg
beginnerProgramWithFlags args = programWithFlags { init = \flags -> (args.init flags, args.main), update = dummyUpdate, subscriptions = args.subscriptions }

{-|Port of *VirtualDom.programWithFlags* with [IO](#IO) (also works with *Html*).-}
beginnerVDomProgramWithFlags : { init : flags -> model,
                                 view : model -> Node (IO model msg),
                                 subscriptions : model -> Sub (IO model msg)
                               } -> Program flags model msg
beginnerVDomProgramWithFlags args = vDomProgramWithFlags { init = \flags -> (args.init flags, none), update = dummyUpdate, subscriptions = args.subscriptions, view = args.view }

{-|Port of *Platform.programWithFlags* with [IO](#IO).-}
programWithFlags : { init : flags -> (model, IO model msg),
                     update : msg -> IO model msg,
                     subscriptions : model -> Sub (IO model msg)
                   } -> Program flags model msg
programWithFlags = transformWithFlags >> Platform.programWithFlags

{-|Port of *VirtualDom.programWithFlags* with [IO](#IO) (also works with *Html*).-}
vDomProgramWithFlags : { init : flags -> (model, IO model msg),
                         update : msg -> IO model msg,
                         subscriptions : model -> Sub (IO model msg),
                         view : model -> Node (IO model msg)
                       } -> Program flags model msg
vDomProgramWithFlags = transformWithFlags >> VirtualDom.programWithFlags