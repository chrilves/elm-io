module IO.Internal exposing (..)


type IO model a
    = Pure a
    | Impure (Base model (IO model a))



-- Utils
{- This type is useful to *get* the model without having to pass
   through a *Cmd*. Accessing the model is an effect, it has to go
   in the *Impure* case. But This is not a *Cmd* effect, it is not
   an effect for the elm runtime. So putting a value `a` into a *Cmd*
   makes no sense! *CmdP* (for *Cmd* + *Pure*) enable to store a pure
   value as an effect without passing in the elm runtime.
-}


type alias Base model a =
    model -> ( model, List a, Cmd a )


baseMap : (a -> b) -> Base model a -> Base model b
baseMap f m s =
    let
        ( s2, l, cmd ) =
            m s
    in
    ( s2, List.map f l, Cmd.map f cmd )
