module CmdM.Internal exposing (Base(..), CmdM(..), baseMap)


type CmdM msg
    = Pure msg
    | Impure (Base (CmdM msg))

-- Utils

type Base a = Batch   (List a)
            | Command (Cmd a)

baseMap : (a -> b) -> Base a -> Base b
baseMap f base =
    case base of
        Batch   l -> Batch   (List.map f l)
        Command c -> Command (Cmd.map f c)