module CmdM exposing (
  CmdM,
  {- Monadic      -} pure, map, andThen, join, ap, seq,
  {- Monoid       -} none, batch, combine, list,
  {- Transformer  -} lift,
  {- Traversal    -} traverse, mapM,
  Program,
  {- Run no flags -} transform, program, vDomProgram,
  {- Run    flags -} transformWithFlags, programWithFlags, vDomProgramWithFlags
 )

{-|This module provides a monadic interface for commands.

Basically [CmdM](#CmdM) is like `Cmd` but is a monad, which means
you can chain effects as you like!
@docs CmdM

# Runing an Elm application with [CmdM](#CmdM)
This module port the four main way of running an Elm application to [CmdM](#CmdM).
@docs Program, program, vDomProgram, programWithFlags, vDomProgramWithFlags

# Lifting values and commands into [CmdM](#CmdM)
@docs pure, lift, none

# Classic monadic operations
@docs map, andThen, join, ap, seq, traverse, mapM

# Transform CmdM into regular Elm
@docs transform, transformWithFlags

# Batch operations
Beware that batch operations might not do what you think. The execution order of
messages and commands is **not defined**.
@docs batch, combine, list
-}

import Platform.Cmd exposing (..)
import VirtualDom exposing (..)
import CmdM.Internal exposing (..)

{-| Monadic interface for commands.

A value of type `CmdM msg` is an effectful
computation that can perform commands and
contains values of type `msg`.
-}

type alias CmdM msg = CmdM.Internal.CmdM msg

-- Monadic

{-|Returns a [CmdM](#CmdM) whose only effect is containing the value given to [pure](#pure).-}
pure : a -> CmdM a
pure a = Pure a

{-|Send messages in batch-}
list : List a -> CmdM a
list l =
  case l of
    [x] -> Pure x
    _   -> Impure (List.map Pure l, Cmd.none)

{-|Transforms an Elm command into a monadic command [CmdM](#CmdM).-}
lift : Cmd a -> CmdM a
lift cmd = Impure ([] , Cmd.map Pure cmd)

{-|Map a function over an [CmdM](#CmdM).

**Laws**
- ```map (f >> g) = (map f) >> (map g)```
- ```map identity = identity```
-}
map : (a -> b) -> CmdM a -> CmdM b
map f =
  let aux cmdm =
        case cmdm of
          Pure a   -> Pure (f a)
          Impure m -> Impure (baseMap aux m)
  in aux

{-|Chains [CmdM](#CmdM)s.

If you have a `CmdM a` and a function which given
a `a` can give you a `CmdM b` depending on the value
of type `a` given to the function. Then [andThen](#andThen) gives you
a `CmdM b` that will run the first [CmdM](#CmdM) and then apply
the function.

**Laws**
- ```andThen pure = identity```
- ```andThen (f >> pure) = map f```
- ```(andThen f) >> (andThen g) = andThen (a -> andThen g (f a))```
-}
andThen : (a -> CmdM b) -> CmdM a -> CmdM b
andThen f =
  let aux m = 
        case m of
          Pure a   -> f a
          Impure x -> Impure (baseMap aux x)
  in aux

{-|Flatten a [CmdM](#CmdM) containing a [CmdM](#CmdM) into a simple [CmdM](#CmdM).

**Laws**
- ```join (pure m) = m```
-}
join : CmdM (CmdM a) -> CmdM a
join = andThen identity

{-|Transform a [CmdM](#CmdM) containing functions into functions on [CmdM](#CmdM).
It enable to easily lift functions to [CmdM](#CmdM).

**Laws**
- ```ap (pure identity) = identity```
- ```ap (pure (f >> g)) = ap (pure f) >> ap (pure g)```
-}
ap : CmdM (a -> b) -> CmdM a -> CmdM b
ap mf ma = andThen (flip  map ma) mf

{-|Run the first argument, ignore the result, then run the second.-}
seq : CmdM a -> CmdM b -> CmdM b
seq = map (\_ -> identity) >> ap

-- Monoid

{-|A [CmdM](#CmdM) doing nothing (an containing no values!).-}
none : CmdM a
none = lift Cmd.none

{-|
**I strongly discourage you from using it. Use [mapM](#mapM) instead.**
Group commands in a batch. Its behavior may not be what you expect!
-}
batch : List (CmdM a) -> CmdM a
batch l =
  let
    accumulate : List (CmdM b) -> List (List (CmdM b)) -> Cmd (CmdM b) -> CmdM b
    accumulate l lists cmd =
      case l of
        []       -> Impure (List.reverse lists |> List.concat, cmd)
        hd :: tl -> case hd of
                      Pure _               -> accumulate tl ([hd]  :: lists) cmd
                      Impure (list2, cmd2) -> accumulate tl (list2 :: lists) (Cmd.batch [cmd, cmd2])
  in case l of
       []  -> none
       [x] -> x
       _   -> accumulate l [] Cmd.none

{-|
**I strongly discourage you from using it. Use [mapM](#mapM) instead.**
Group commands in a batch. Its behavior may not be what you expect!
-}
combine : CmdM a -> CmdM a -> CmdM a
combine x y = batch [x,y]

{-|You can think of traverse like a [map](#map) but with effects.
It maps a function performing [CmdM](#CmdM) effects over a list.
-}
traverse : (a -> CmdM b) -> List a -> CmdM (List b)
traverse f l =
  case l of
    []       -> pure []
    hd :: tl -> ap (ap (pure (::)) (f hd)) (traverse f tl)

{-|Transform a list of [CmdM](#CmdM) into an [CmdM](#CmdM) of list.-}
mapM : List (CmdM a) -> CmdM (List a)
mapM = traverse identity

-- Platform

{-|Program using [CmdM](#CmdM).-}
type alias Program flags model msg = Platform.Program flags model (CmdM msg)

-- The core of all the [CmdM](#CmdM) monad! It runs the [CmdM](#CmdM) monad using the update function.
runUpdate : (msg -> model -> (model, CmdM msg)) -> CmdM msg -> model -> (model, Cmd (CmdM msg))
runUpdate f cmdm =
  let aux : List (CmdM msg) -> Cmd (CmdM msg) -> model -> (model, Cmd (CmdM msg))
      aux l cmd m =
        case l of
          []       -> (m, cmd)
          hd :: tl -> case hd of
                        Pure msg             -> let (model2, cmdm2) = f msg m
                                                in aux (cmdm2 :: tl) cmd model2
                        Impure (list2, cmd2) -> aux (list2 ++ tl) (Cmd.batch [cmd, cmd2]) m
  in aux [cmdm] Cmd.none

{-|Transform a program using [CmdM](#CmdM) into a normal program.-}
transform :   { y | init : (model,      CmdM msg),  update :      msg -> model -> (model,       CmdM msg ) }
           -> { y | init : (model, Cmd (CmdM msg)), update : CmdM msg -> model -> (model , Cmd (CmdM msg)) }
transform args =
  let
    update : CmdM msg -> model -> (model, Cmd (CmdM msg))
    update = runUpdate args.update

    init : (model, Cmd (CmdM msg))
    init = let (model0, cmdm0) = args.init
           in update cmdm0 model0

  in { args | init = init, update = update }

{-|Port of *Platform*.program with [CmdM](#CmdM).-}
program : { init : (model, CmdM msg),
            update : msg -> model -> (model, CmdM msg),
            subscriptions : model -> Sub (CmdM msg)
          } -> Program Never model msg
program = transform >> Platform.program

{-|Port of *VirtualDom*.program with [CmdM](#CmdM) (also works with *HTML*).-}
vDomProgram : { init : (model, CmdM msg),
                update : msg -> model -> (model , CmdM msg),
                subscriptions : model -> Sub (CmdM msg),
                view : model -> Node (CmdM msg)
              } -> Program Never model msg
vDomProgram = transform >> VirtualDom.program


{-|Transform a program using [CmdM](#CmdM) into a normal program.-}
transformWithFlags :   { y | init : flags -> (model, CmdM msg), update : msg -> model -> (model, CmdM msg) }
                    -> { y | init : flags -> (model, Cmd (CmdM msg)), update : CmdM msg -> model -> (model, Cmd (CmdM msg))}
transformWithFlags args =
  let
    update : CmdM msg -> model -> (model, Cmd (CmdM msg))
    update = runUpdate args.update

    init : flags -> (model, Cmd (CmdM msg))
    init flags = let (model0, cmdm0) = args.init flags
                 in update cmdm0 model0

  in { args | init = init, update = update }


{-|Port of *Platform.programWithFlags* with [CmdM](#CmdM).-}
programWithFlags : { init : flags -> (model, CmdM msg),
                     update : msg -> model -> (model, CmdM msg),
                     subscriptions : model -> Sub (CmdM msg)
                   } -> Program flags model msg
programWithFlags = transformWithFlags >> Platform.programWithFlags

{-|Port of *VirtualDom.programWithFlags* with [CmdM](#CmdM) (also works with *Html*).-}
vDomProgramWithFlags : { init : flags -> (model, CmdM msg),
                         update : msg -> model -> (model , CmdM msg),
                         subscriptions : model -> Sub (CmdM msg),
                         view : model -> Node (CmdM msg)
                       } -> Program flags model msg
vDomProgramWithFlags = transformWithFlags >> VirtualDom.programWithFlags