module IO exposing (
  IO,
  {- Monadic      -} pure, map, bind, join, ap, 
  {- Monoid       -} none, batch, combine, list,
  {- Transformer  -} lift, liftUpdate,
  {- State        -} get, set, modify,
  {- Optics       -} lens, select,
  {- Traversal    -} traverse, mapM,
  {- Dummy        -} dummyUpdate, dummySub,
  Program,
  {- Run no flags -} transform, beginnerProgram, beginnerVDomProgram, program, vDomProgram,
  {- Run    flags -} transformWithFlags, beginnerProgramWithFlags, beginnerVDomProgramWithFlags, programWithFlags, vDomProgramWithFlags
 )

{-|This module provides a monadic interface for *The Elm Architecture*.

Basically *IO* is a monad enabing two kinds of effects :
- model modification (it is a state monad)
- *Cmd* commands
@docs IO

# Runing an Elm application with *IO*
This module port the two main ways of running an Elm application to *IO*.
@docs Program, beginnerVDomProgram, vDomProgram, beginnerVDomProgramWithFlags, vDomProgramWithFlags

# Lifting values and commands into *IO*
@docs pure, lift, liftUpdate

# The model as a state
@docs get, set, modify

# Classic monadic operations
@docs map, bind, join, ap, traverse, mapM

# Passing from a model to another
@docs lens, select

# Dummy values
@docs none, dummyUpdate, dummySub

# Transform IO into regular Elm
@docs transform, transformWithFlags

# Runing an headless Elm application with *IO*
This module port the four main ways of running an headless Elm application to *IO*.
@docs beginnerProgram, program, beginnerProgramWithFlags, programWithFlags

# Batch operations
@docs batch, combine, list
-}

import Platform.Cmd exposing (..)
import Lens exposing (..)
import Select exposing (..)
import VirtualDom exposing (..)
import CmdM exposing (..)

{-|Monadic interface for *The Elm Architecture*.

A value of type `IO model a` is an effectful
computation that can modify the model `model`,
perform commands and contains values of type `a`.
-}
type IO model a = Pure a
                | Impure (Base model (IO model a))


-- Utils

{- This type is useful to *get* the model without having to pass
through a *Cmd*. Accessing the model is an effect, it has to go
in the *Impure* case. But This is not a *Cmd* effect, it is not
an effect for the elm runtime. So putting a value `a` into a *Cmd*
makes no sense! *CmdP* (for *Cmd* + *Pure*) enable to store a pure
value as an effect without passing in the elm runtime.
-}
type alias Base model a = model -> (model, List a, Cmd a)

baseMap : (a -> b) -> Base model a -> Base model b
baseMap f m s = let (s2, l, cmd) = m s in (s2, List.map f l, Cmd.map f cmd)

-- Monadic

{-|Returns an *IO* whose only effect is containing the value given to *pure*.-}
pure : a -> IO model a
pure a = Pure a

{-|Send messages in batch mode-}
list : List a -> IO model a
list l =
  case l of
    [x] -> pure x
    _   -> Impure (\model -> (model, List.map Pure l, Cmd.none))

{-|Map a function over an *IO*.

**Laws**
- ```map (f >> g) = (map f) >> (map g)```
- ```map identity = identity```
-}
map : (a -> b) -> IO model a -> IO model b
map f ioa =
  case ioa of
    Pure a   -> Pure (f a)
    Impure m -> Impure (baseMap (map f) m)


{-|Chains *IO*s.

If you have an *IO model a* and a function which given
a *a* can give you an *IO model b* depending on the value
of type *a* given to the function. Then *bind* gives you
an *IO model b* that will run the first *IO* and then apply
the function.

**Laws**
- ```bind pure = identity```
- ```bind (f >> pure) = map f```
- ```(bind f) >> (bind g) = bind (a -> bind g (f a))```
-}
bind : (a -> IO model b) -> IO model a -> IO model b
bind f m =
  case m of
    Pure a   -> f a
    Impure x -> Impure (baseMap (bind f) x)

{-|Flatten an *IO* containing an *IO* into a simple *IO*.

**Laws**
- ```join (pure m) = m```
-}
join : IO model (IO model a) -> IO model a
join = bind identity

{-|Transform an *IO* containing functions into functions on *IO*
It enable to easily lift functions to *IO*.

**Laws**
- ```ap (pure identity) = identity```
- ```ap (pure (f >> g)) = ap (pure f) >> ap (pure g)```
-}
ap : IO model (a -> b) -> IO model a -> IO model b
ap mf ma = bind (flip  map ma) mf

-- Monoid

{-|An *IO* doing nothing (an containing no values!).-}
none : IO model a
none = lift Cmd.none

{-|Combine a list of *IO*. Its use is strongly discouraged!
Use *mapM* instead!
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

{-|Combine two *IO*. Its use is strongly discouraged!
Use *mapM instead!
-}
combine : IO model a -> IO model a -> IO model a
combine x y = batch [x,y]

-- Tansformer

{-|Lift a *Cmd* as an  *IO*.-}
lift : Cmd a -> IO model a
lift cmd = Impure (\s -> (s, [], Cmd.map Pure cmd))

{-|Lift a classic update function into an *IO*.-}
liftUpdate : (model -> (model, Cmd a)) -> IO model a
liftUpdate f = Impure (\m -> let (m2, cmd) = f m
                             in (m2, [], Cmd.map Pure cmd)
                      )

-- State

{-|An *IO* that returns the current model.-}
get : IO model model
get = Impure (\s -> (s, [Pure s], Cmd.none))

{-|An *IO* that sets the model.-}
set : model -> IO model ()
set s = Impure (\_ -> (s, [Pure ()], Cmd.none))

{-|A *IO* that modify the model.-}
modify : (model -> model) -> IO model ()
modify f = Impure (\s -> (f s, [Pure ()], Cmd.none))

-- Optics

{-|Congruence by a *Lens* on an *IO*.

It would be silly to force users to redefine every *IO*
for each application model. Lenses enable to lift an *IO*
action on a model *a* to the same *IO* but action on a
model *b*.

You can then define your *IO* on the minimal model and
lift them to you real application's model when needed.
-}
lens : Lens a b -> IO a msg -> IO b msg
lens l iob =
  case iob of
    Pure msg -> Pure msg
    Impure x -> Impure (\a -> let (b, context) = l a
                                  (b2, list, cmd)   = x b
                                  f = lens l
                              in (context b2, List.map f list, Cmd.map f cmd)
                       )

{-|Congruence by a *Select* on an *IO*.
Just like lenses but with *Select*.
-}
select : Select a b -> IO a msg -> IO b msg
select l ioa =
  case ioa of
    Pure msg -> Pure msg
    Impure x -> Impure (\b -> case l b of
                                Nothing           -> (b, [], Cmd.none)
                                Just (a, context) -> let (a2, list, ioa) = x a
                                                         f = select l
                                                     in (context a2, List.map f list, Cmd.map f ioa)
                       )

{-|You can think of traverse like a *map* but with effects.
It maps a function performing *IO* effects over a list.
-}
traverse : (a -> IO model b) -> List a -> IO model (List b)
traverse f l =
  case l of
    []       -> pure []
    hd :: tl -> ap (ap (pure (::)) (f hd)) (traverse f tl)

{-|Transform a list of `IO` into an `IO` of list.-}
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

{-|Program using *IO*.-}
type alias Program flags model msg = Platform.Program flags model (IO model msg)

-- The core of all the *IO* monad! It runs the *IO* monad using the update function.
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


{-|Transform a program using *IO* into a normal program.-}
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

{-|Port of *Platform.program* with *IO* with *dummyUpdate*.-}
beginnerProgram : { init  : model,
                    main  : IO model msg,
                    subscriptions : model -> Sub (IO model msg)
                  } -> Program Never model msg
beginnerProgram args = program { init = (args.init, args.main), update = dummyUpdate, subscriptions = args.subscriptions }

{-|Port of *VirtualDom.program* with *IO* with *dummyUpdate* (also works with *Html*).-}
beginnerVDomProgram : { init  : model,
                        view  : model -> Node (IO model msg),
                        subscriptions : model -> Sub (IO model msg)
                      } -> Program Never model msg
beginnerVDomProgram args = vDomProgram { init = (args.init, none), update = dummyUpdate, subscriptions = args.subscriptions, view = args.view }

{-|Port of *Platform.program* with *IO*.-}
program : { init : (model, IO model msg),
            update : msg -> IO model msg,
            subscriptions : model -> Sub (IO model msg)
          } -> Program Never model msg
program = transform >> Platform.program

{-|Port of *VirtualDom.program* with *IO* (also works with *Html*).-}
vDomProgram : { init : (model, IO model msg),
                update : msg -> IO model msg,
                subscriptions : model -> Sub (IO model msg),
                view : model -> Node (IO model msg)
              } -> Program Never model msg
vDomProgram = transform >> VirtualDom.program


{-|Transform a program using *IO* into a normal program.-}
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


{-|Port of *Platform.programWithFlags* with *IO* with *dummyUpdate*.-}
beginnerProgramWithFlags : { init  : flags -> model,
                             main  : IO model msg,
                             subscriptions : model -> Sub (IO model msg)
                           } -> Program flags model msg
beginnerProgramWithFlags args = programWithFlags { init = \flags -> (args.init flags, args.main), update = dummyUpdate, subscriptions = args.subscriptions }

{-|Port of *VirtualDom.programWithFlags* with *IO* (also works with *Html*).-}
beginnerVDomProgramWithFlags : { init : flags -> model,
                                 view : model -> Node (IO model msg),
                                 subscriptions : model -> Sub (IO model msg)
                               } -> Program flags model msg
beginnerVDomProgramWithFlags args = vDomProgramWithFlags { init = \flags -> (args.init flags, none), update = dummyUpdate, subscriptions = args.subscriptions, view = args.view }

{-|Port of *Platform.programWithFlags* with *IO*.-}
programWithFlags : { init : flags -> (model, IO model msg),
                     update : msg -> IO model msg,
                     subscriptions : model -> Sub (IO model msg)
                   } -> Program flags model msg
programWithFlags = transformWithFlags >> Platform.programWithFlags

{-|Port of *VirtualDom.programWithFlags* with *IO* (also works with *Html*).-}
vDomProgramWithFlags : { init : flags -> (model, IO model msg),
                         update : msg -> IO model msg,
                         subscriptions : model -> Sub (IO model msg),
                         view : model -> Node (IO model msg)
                       } -> Program flags model msg
vDomProgramWithFlags = transformWithFlags >> VirtualDom.programWithFlags
