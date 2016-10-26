
-- | We will explore Yoneda embeddings in a self-enriched CCC.
module PresheafCategory where

import Prelude

-- The Yoneda embedding maps C to its category of presheaves:
-- objects `a` map to their representable presheaves `Y a` (functors)
-- morphisms `f : a -> b` map to morphisms on presheaves `mapY f : Y a ~> Y b` (natural transformations)

-- Because we are working in a C-enriched category, a representable presheaf on A
-- is equivalent to a contravariant functor from C to 

type Y cod dom = dom -> cod

-- | The Yoneda embedding is functorial:
mapY :: forall a b. (a -> b) -> (Y a ~> Y b)
mapY = (<<<)

-- | The Yoneda embedding is invertible:
unmapY :: forall a b. (Y a ~> Y b) -> (a -> b)
unmapY nat = nat id

type Isomorphism a b = { to :: a -> b , from :: b -> a}

-- | The Yoneda embedding is fully faithful
-- | i.e. our hom spaces are bijective:
isoY :: forall a b. Isomorphism (a -> b) (Y a ~> Y b)
isoY = { to: mapY, from: unmapY}

-- | One perspective is that the Yoneda embedding gives us functorality of all readers...
mapReader :: forall a b r. (a -> b)  -> (r -> a) -> (r -> b)
mapReader = mapY

-- | ... (notice mapY coincides with the Prelude instance for `map` on readers, aka `<<<`)
mapReader' :: forall a b r. (a -> b) -> (r -> a) -> (r -> b)
mapReader' = map


-- A reader monad from A is a context of dependence on A.
-- A term of type `A -> B`, i.e. a term of `Reader A B`,
-- is a computation of type B that **depends** on a term of type A

-- The fact that Y is fully faithful tells us that we do not lose any information
-- when lifting our functions to some context of dependence.
-- In other words, it is a proof that dependency injection is sound.
