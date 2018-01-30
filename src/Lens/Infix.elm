module Lens.Infix exposing (..)

{-|Infix notation for [Lens](../Lens)
@docs (<°>)
-}

import Lens exposing (..)

{-|Infix notation for [Lens.compose](../Lens#compose).-}
(<°>) : Lens a b -> Lens b c -> Lens a c
(<°>) = Lens.compose