module CmdM.Infix exposing (..)

{-|Infix notation for [CmdM](../CmdM)
@docs (<$>), (>>=), (<*>), (<+>), (<!>)
-}

import CmdM exposing (..)

{-|Infix notation for [CmdM.map](../CmdM#map).-}
(<$>) : CmdM a -> (a -> b) -> CmdM b
(<$>) m f = CmdM.map f m

{-|Infix notation for [CmdM.bind](../CmdM#bind).-}
(>>=) : CmdM a -> (a -> CmdM b) -> CmdM b
(>>=) m f = CmdM.bind f m

{-|Infix notation for [CmdM.ap](../CmdM#ap).-}
(<*>) : CmdM (a -> b) -> CmdM a -> CmdM b
(<*>) = CmdM.ap

{-|Infix notation for [CmdM.combine](../CmdM#combine).-}
(<+>) : CmdM a -> CmdM a -> CmdM a
(<+>) = CmdM.combine

{-|Infix notation for [CmdM.seq](../CmdM#seq).-}
(<!>) : CmdM a -> CmdM b -> CmdM b
(<!>) = CmdM.seq