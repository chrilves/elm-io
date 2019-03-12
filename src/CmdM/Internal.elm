module CmdM.Internal exposing (Effect(..), CmdM(..), effectMap)


type CmdM msg
    = Pure msg
    | Impure (Effect (CmdM msg))

-- Utils

type Effect a = Batch   (List a)
              | Command (Cmd a)

effectMap : (a -> b) -> Effect a -> Effect b
effectMap f base =
    case base of
        Batch   l -> Batch   (List.map f l)
        Command c -> Command (Cmd.map f c)