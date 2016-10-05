module Records where
import Prelude

type Semigroup a rest = Record (add :: a -> a -> a | rest)
type Monoid a rest = Semigroup a (zero :: a | rest)

additiveMonoid :: Monoid Number ()
additiveMonoid = {zero, add}
  where
    zero = 0.0
    add = (+)

multiplicativeMonoid :: Monoid Number ()
multiplicativeMonoid = {zero, add}
  where
    zero = 1.0
    add = (*)

test :: Semigroup Number ()
test = { add: (+) }

foo :: Record ()
foo = {}

-- | This would be nice :)
{-- shouldBeId :: forall a rest. Monoid a rest -> Monoid a rest --}
{-- shouldBeId {zero, add | rest} = {zero, add | rest} --}

shouldBeId :: forall a rest. Monoid a rest -> Monoid a rest
shouldBeId r@{zero, add} = r {zero = add zero zero, add = add}

shouldBeId' :: forall a rest. Monoid a rest -> Monoid a rest
shouldBeId' r = r {zero=zero, add=add}
  where
    zero = case r of
      {zero, add} -> zero
    add = case r of
      {zero, add} -> add

cayleyRep :: forall a rest. Monoid a rest -> Monoid (a -> a) rest
cayleyRep r = r {zero=zero, add=add}
  where
    zero = case r of
      {zero, add} -> add zero
    add = (<<<)

-- | This is an ml functor!
makeMonoid :: forall a rest. a -> Semigroup a rest -> Monoid a ()
makeMonoid zero {add} = {zero: zero, add: add}

-- How would you take away record fields?
-- An issue is that `rest` might contains type variables:
{-- escape :: forall a rest. Monoid a rest -> Record rest --}
{-- escape r@{zero, add} = r --}

type Semiring a rest = Monoid a
  ( one :: a
  , mul :: a -> a -> a
  | rest
  )

makeSemiring :: forall a rest. Monoid a rest -> Monoid a rest -> Semiring a ()
makeSemiring {zero, add} m =
  { zero
  , add
  , one: m.zero
  , mul: m.add
  }

numberSemiring :: Semiring Number ()
numberSemiring = makeSemiring additiveMonoid multiplicativeMonoid
