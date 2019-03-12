module IO.Internal exposing (Effect(..), IO(..), effectMap)

import CmdM.Internal as CmdM

type IO model a
    = Pure a
    | Impure (Effect model (IO model a))

-- Utils
{- This type is useful to *get* the model without having to pass
   through a *Cmd*. Accessing the model is an effect, it has to go
   in the *Impure* case. But This is not a *Cmd* effect, it is not
   an effect for the elm runtime. So putting a value `a` into a *Cmd*
   makes no sense! *CmdP* (for *Cmd* + *Pure*) enable to store a pure
   value as an effect without passing in the elm runtime.
-}


type Effect model a = Get (model -> a)
                    | Set model (() -> a)
                    | Batch (List a)
                    | Command (Cmd a)

effectMap : (a -> b) -> Effect model a -> Effect model b
effectMap f b =
    case b of
        Get k     -> Get (k >> f) 
        Set m k   -> Set m (k >> f)
        Batch l   -> Batch (List.map f l)
        Command c -> Command (Cmd.map f c)