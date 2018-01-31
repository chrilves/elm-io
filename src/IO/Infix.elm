module IO.Infix exposing (..)

{-|Infix notation for [IO](../IO)
@docs (<$>), (>>=), (<*>), (<+>), (<!>)
-}

import IO exposing (..)

{-|Infix notation for [IO.map](../IO#map).-}
(<$>) : IO model a -> (a -> b) -> IO model b
(<$>) m f = IO.map f m

{-|Infix notation for [IO.andThen](../IO#andThen).-}
(>>=) : IO model a -> (a -> IO model b) -> IO model b
(>>=) m f = IO.andThen f m

{-|Infix notation for [IO.ap](../IO#ap).-}
(<*>) : IO model (a -> b) -> IO model a -> IO model b
(<*>) = IO.ap

{-|Infix notation for [IO.combine](../IO#combine).-}
(<+>) : IO model a -> IO model a -> IO model a
(<+>) = IO.combine

{-|Infix notation for [IO.seq](../IO#seq).-}
(<!>) : IO model a -> IO model b -> IO model b
(<!>) = IO.seq

