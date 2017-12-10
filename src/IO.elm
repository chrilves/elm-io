module IO exposing (
  IO,
  {- Monadic      -} pure, map, bind, join, ap, 
  {- Monoid       -} none, batch, combine,
  {- Transformer  -} lift, liftM, liftUpdate,
  {- State        -} get, set, modify,
  {- Optics       -} lens, select,
  {- Traversal    -} traverse, mapM,
  Program,
  {- Run no flags -} transform, program, vDomProgram,
  {- Run    flags -} transformWithFlags, programWithFlags, vDomProgramWithFlags
 )

{-|This module provides a monadic interface for *The Elm Architecture*.

Basically *IO* is a monad enabing two kinds of effects :
- model modification (it is a state monad)
- *Cmd* commands
@docs IO

# Runing an Elm application with *IO*
This module port the four main way of running an Elm application to *IO*.
@docs Program, program, vDomProgram, programWithFlags, vDomProgramWithFlags

# Lifting values and commands into *IO*
@docs pure, lift, liftM, liftUpdate, none

# The model as a state
@docs get, set, modify

# Classic monadic operations
@docs map, bind, join, ap, traverse, mapM

# Passing from a model to another
@docs lens, select

# Transform IO into regular Elm
@docs transform, transformWithFlags

# Unsafe operations (You've been warned!)
@docs batch, combine
-}

import Platform.Cmd exposing (..)
import Task exposing (..)
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
                | Impure (IOBase model (IO model a))

type alias IOBase model a = model -> (model, CmdP a)

-- Utils

{- This type is useful to *get* the model without having to pass
through a *Cmd*. Accessing the model is an effect, it has to go
in the *Impure* case. But This is not a *Cmd* effect, it is not
an effect for the elm runtime. So putting a value `a` into a *Cmd*
makes no sense! *CmdP* (for *Cmd* + *Pure*) enable to store a pure
value as an effect without passing in the elm runtime.
-}
type CmdP a = CmdPPure a
            | CmdPCmd (Cmd a)

cmdPmap : (a -> b) -> CmdP a -> CmdP b
cmdPmap f p =
  case p of
    CmdPPure a  -> CmdPPure (f a)
    CmdPCmd cmd -> CmdPCmd (Cmd.map f cmd)

cmdPnone : CmdP a
cmdPnone = CmdPCmd Cmd.none

-- Monadic

{-|Returns an *IO* whose only effect is containing the value given to *pure*.-}
pure : a -> IO model a
pure a = Pure a

{-|Map a function over an *IO*.

**Laws**
- ```map (f >> g) = (map f) >> (map g)```
- ```map identity = identity```
-}
map : (a -> b) -> IO model a -> IO model b
map f ioa =
  case ioa of
    Pure a   -> Pure (f a)
    Impure m -> Impure (\s -> let (s2, cmd) = m s
                              in (s2, cmdPmap (map f) cmd)
                       )


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
    Impure x -> Impure (\s -> let (s2, cmdp) = x s
                              in  (s2, cmdPmap (bind f) cmdp)
                       )

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
    pureCmd : b -> Cmd b
    pureCmd a = Task.perform identity (Task.succeed a)

    reify : IO model b -> model -> (model, Cmd (IO model b))
    reify cmd =
      case cmd of
        Pure b   -> \s -> (s, pureCmd (Pure b))
        Impure c -> \s -> let (s2, cmdp) = c s
                          in case cmdp of
                               CmdPPure p -> (s2, pureCmd p)
                               CmdPCmd  x -> (s2, x)

    accumulate : List (model -> (model, Cmd b)) -> List (Cmd b) -> IOBase model b
    accumulate l cmds model =
      case l of
        []       -> (model, CmdPCmd (Cmd.batch (List.reverse cmds)))
        hd :: tl -> let (model2, cmd2) = hd model
                    in accumulate tl (cmd2 :: cmds) model

  in Impure (accumulate (List.map reify l) [])

{-|Combine two *IO*. Its use is strongly discouraged!
Use *mapM instead!
-}
combine : IO model a -> IO model a -> IO model a
combine x y = batch [x,y]

-- Tansformer

{-|Lift a *Cmd* as an  *IO*.-}
lift : Cmd a -> IO model a
lift cmd = Impure (\s -> (s, CmdPCmd (Cmd.map Pure cmd)))

{-|Lift a *CmdM* as an *IO*.-}
liftM : CmdM a -> IO model a
liftM = CmdM.lazyFold Pure (\cmdiof -> Impure (\s -> (s, CmdPCmd (cmdiof ()))))

{-|Lift a classic update function into an *IO*.-}
liftUpdate : (model -> (model, Cmd a)) -> IO model a
liftUpdate f = Impure (\m -> let (m2, cmd) = f m
                             in (m2, CmdPCmd (Cmd.map Pure cmd))
                      )

-- State

{-|An *IO* that returns the current model.-}
get : IO model model
get = Impure (\s -> (s, CmdPPure (Pure s)))

{-|An *IO* that sets the model.-}
set : model -> IO model ()
set s = Impure (\_ -> (s, cmdPnone))

{-|A *IO* that modify the model.-}
modify : (model -> model) -> IO model ()
modify f = Impure (\s -> (f s, cmdPnone))

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
                                  (b2, cmdp)   = x b
                              in (context b2, cmdPmap (lens l) cmdp)
                       )

{-|Congruence by a *Select* on an *IO*.
Just like lenses but with *Select*.
-}
select : Select a b -> IO a msg -> IO b msg
select l ioa =
  case ioa of
    Pure msg -> Pure msg
    Impure x -> Impure (\b -> case l b of
                                Nothing           -> (b, cmdPnone)
                                Just (a, context) -> let (a2, ioa) = x a
                                                     in (context a2, cmdPmap (select l) ioa)
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

-- Platform

{-|Program using *IO*.-}
type alias Program flags model msg = Platform.Program flags model (IO model msg)

-- The core of all the *IO* monad! It runs the *IO* monad using the update function.
runUpdate : (msg -> IO model msg) -> IO model msg -> model -> (model, Cmd (IO model msg))
runUpdate f io0 model =
  case io0 of
    Pure msg -> runUpdate f (f msg) model
    Impure x -> let (s2, cmdp) = x model
                in case cmdp of
                     CmdPPure io1 -> runUpdate f io1 s2
                     CmdPCmd  cmd -> (s2, cmd)


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