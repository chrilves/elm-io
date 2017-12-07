module Lens exposing (Lens, lift, compose, congruence)

{-| Imagine the type `a = {name: String, age : Int}`.
If you want to access the `age` part of this type
or modify only the `age` this is simple you would use
**Elm** syntax like `x.age` or `{x | age = 7 }` to ease
the process.

Now imagine that the `a` is much much more complex, like
a record whose fields are also records and so on for 10 levels.
**Elm** does not have buitin syntax to ease this case. That's
where lenses comes in.

A lens is like zooming on a part of a type, enabling to access
or modify this part with ease. It is strongly related to Huet's
zipper.

Take the a lens of type `Lens a b`. It enable to access the `a`
part of `b` and modify it simply. The benefits of lenses is they
compose naturally.

@docs Lens, lift, compose, congruence
-}

{-| The type of a lens -}
type alias Lens   a b = b -> (a, a -> b)

{-| Build a lens from an equivalence between types `a` and `b` -}
lift : (a -> b) -> (b -> a) -> Lens a b
lift f g b = (g b, f)

{-| Compose two lenses -}
compose : Lens a b -> Lens b c -> Lens a c
compose lab lbc c =
  let (b, f) = lbc c
      (a, g) = lab b
  in (a, g >> f)

{-| If you have a lens from `a` to `b` and a function on `a`
then you get a function on `b`.
-}
congruence : Lens a b -> (a -> a) -> (b -> b)
congruence l f b = let (a, g) = l b in g (f a)