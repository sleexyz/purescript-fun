module Peano where


import Prelude


-- No data kinds...

-- | encode via rows

data Zero
data Succ a = Succ a

data Mu f = In f (Mu f)



-- data Nat (constraints :: # *) = Nat Number

-- mkNat :: Number -> Nat (value :: Zero, censored :: Censored)
-- mkNat a = Nat a



data Nat (constraints :: # *) = Nat

zilch :: Nat (size :: Zero)
zilch = Nat

one :: Nat (size :: Succ Zero)
one = Nat

succ :: forall num. Nat (size :: num) -> Nat (size :: Succ num)
succ n = Nat

