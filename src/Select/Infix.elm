module Select.Infix exposing (..)

{-|Infix notation for `Select`
@docs (<°>)
-}

import Select exposing (..)

{-|Infix notation for `Select.compose`-}
(<°>) : Select a b -> Select b c -> Select a c
(<°>) = Select.compose