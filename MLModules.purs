module MLModules where

import Prelude hiding (map)

-- * Some boilerplate

-- | Existentials without existential types/GADTs via De Morgan's law
-- | 
-- | If purescript had polykinds, we can hide the row...
newtype Exists p = Exists (forall b. (forall a. p a -> b) -> b)

pack :: forall p a. p a -> Exists p
pack x = Exists (\f -> f x)

elim :: forall p b. (forall a. p a -> b) -> Exists p -> b
elim f (Exists k) = k f

transform :: forall p s. (forall a. p a -> s a) -> Exists p -> Exists s
transform f (Exists k) = Exists (\g -> k (g <<< f))

newtype Close (f :: # * -> *) = Close (f ())

-- * Signatures are open records

type Functor f r = Record (
  map :: forall a b. (a -> b) -> f a -> f b | r
)

type Monad f r = Functor f ( 
  pure :: forall a. a -> f a,
  join :: forall a. f (f a) -> f a | r
)

type Monoid a r = Record (
  zero :: a,
  add :: a -> a -> a | r
)


-- * Structures are closed records

data List a = Nil | Cons a (List a)

list = {map, pure, zero, add, join}
  where
    map :: forall a b . (a -> b) -> List a -> List b
    map f Nil = Nil
    map f (Cons x xs') = Cons (f x) (map f xs')

    pure :: forall a. a -> List a
    pure x = Cons x Nil

    zero :: forall a. List a
    zero = Nil

    add :: forall a. List a -> List a -> List a
    add Nil ys = ys
    add (Cons x xs) ys = Cons x (add xs ys)

    join :: forall a. List (List a) -> List a
    join Nil = Nil
    join (Cons x xss) = add x (join xss)

-- | First class Functor instance for lists
-- | fixme: get working
{-- listFunctor :: Exists --} 
{-- listFunctor = pack Nil --}


-- * Modular typeclasses

class HasFunctor (f :: * -> * ) where
  map :: forall a b. (a -> b) -> f a -> f b

