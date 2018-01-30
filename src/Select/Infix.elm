module Select.Infix exposing (..)

{-|Infix notation for  [Select](../Select)
@docs (<°>)
-}

import Select exposing (..)

{-|Infix notation for [Select](../Select#compose).-}
(<°>) : Select a b -> Select b c -> Select a c
(<°>) = Select.compose