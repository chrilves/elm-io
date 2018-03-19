module CmdM.Internal exposing (..)


type CmdM msg
    = Pure msg
    | Impure (Base (CmdM msg))



-- Utils


type alias Base a =
    ( List a, Cmd a )


baseMap : (a -> b) -> Base a -> Base b
baseMap f ( l, cmd ) =
    ( List.map f l, Cmd.map f cmd )
