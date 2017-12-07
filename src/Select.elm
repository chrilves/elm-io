module Select exposing (Select, lift, nothing, compose, congruence)

{-| Selects are a lot like lenses but in a select
from type `a` to `b`, the `a` part may not exist
in `b`.

@docs Select, lift, nothing, compose, congruence
-}

import Lens exposing (..)

{-| The type of a slects -}
type alias Select a b = b -> Maybe (a, a -> b)

{-| Build a lens from an equivalence between types `a` and `b` -}
lift : Lens a b -> Select a b
lift l b = Just (l b)

{-| A select that always fails |-}
nothing : Select a b
nothing b = Nothing

{-| Compose two selects -}
compose : Select a b -> Select b c -> Select a c
compose lab lbc c =
  case lbc c of
    Nothing     -> Nothing
    Just (b, f) -> case lab b of
                     Nothing     -> Nothing
                     Just (a, g) -> Just (a, g >> f)

{-| If you have a lens from `a` to `b` and a function on `a`
then you get a function on `b`.
-}
congruence : Select a b -> (a -> a) -> (b -> b)
congruence l f b =
  case l b of
    Nothing     -> b
    Just (a, g) -> g (f a)