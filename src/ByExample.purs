module ByExample where

import Prelude
import Math
import Data.Generic
import Data.Monoid
import Data.Monoid.Additive
import Data.Maybe

import Control.Apply
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console hiding (error)
import Control.Monad.Eff.Exception
import Control.Monad.ST

-- 3.3 records
author :: {name :: String, interests :: Array String}
author = { name : "Sean", interests : ["poop", "poop"]}


data Point = Point { x :: Number, z :: Number}






-- 6.10 Monoid Actions!

class (Monoid m) <= Action m a where
  act :: m -> a -> a

-- |Can we make a monoid out of a semiring?
-- |Yes, by extracting the Additive part from a semigroup!

instance repeatAction :: Action (Additive Int) String where
  act (Additive 0) _ = ""
  act (Additive n) s = s ++ act (Additive $ n - 1) s


instance fooAction :: (Action m a) => Action m (Array a) where
  act m arr = (act m) <$> arr

newtype Self m = Self m

instance showSelf :: (Show m) => Show (Self m) where
  show (Self x) = "Self " ++ show x


instance selfAction :: (Monoid m) => Action m (Self m) where
  act m (Self n) = Self (m ++ n)

instance monoidAction :: (Monoid m) => Action m m where
  act m n = m ++ n


exSelfAction :: Self (Additive Int)
exSelfAction = act (Additive 2) (Self $ Additive 3)
exMonoidAction :: Additive Int
exMonoidAction = act (Additive 2) (Additive 3)
exFooAction :: Array (Additive Int)
exFooAction = act (Additive 10) [Additive 20, Additive 30]






-- 8.8 Native Effects through Eff monad

main1 :: forall e. Eff (random :: RANDOM, console :: CONSOLE | e) Unit
main1 = do
  n <- random
  print n





-- 8.17 Mutable State

simulate' :: forall eff h. Number -> Number -> Number -> Eff (st :: ST h | eff) Number
simulate' x0 v0 time = do
  ref <- newSTRef { x: x0, v: v0}
  forE 0.0  (time * 1000.0) $ \i -> do
    modifySTRef ref ( \o ->
                       { v: o.v - 9.81 * 0.001
                       , x: o.x + o.v * 0.001
                       }
                    )
    return unit
  final <- readSTRef ref
  return final.x

simulate :: Number -> Number -> Number -> Number
simulate x0 v0 time = runPure $ runST (simulate' x0 v0 time)


safeDivide' :: Int -> Int -> Maybe Int
safeDivide' _ 0 = Nothing
safeDivide' a b = Just ( a / b)

safeDivide :: forall eff. Int -> Int -> Eff (err :: EXCEPTION | eff) Int
safeDivide a b = case safeDivide' a b  of
  Nothing -> throwException $ error "Divide by zero error!"
  Just y -> return y




-- via montecarlo integration

data Coord = Coord { x :: Number, y :: Number}
derive instance genericCoord :: Generic Coord
instance showCoord :: Show Coord where show = gShow

-- estimatePi :: forall h eff . Eff (st :: ST h, random :: RANDOM | eff) Number
-- estimatePi = do
--   ref <- newSTRef 0.0
--   readSTRef ref

estimatePi :: forall eff . Eff (random :: RANDOM | eff) Number
estimatePi = runST (do
                       ref <- newSTRef 0.0
                       forE 0.0 iterations $ \i -> do
                         x <- random
                         y <- random
                         when (norm x y < 1.0) $ do
                                             modifySTRef ref (\o -> o + 1.0)
                                             return unit
                         return unit
                       final <- readSTRef ref
                       return $ final / iterations * 4.0
                   )
  where
    iterations :: Number
    iterations = 1000000.0

    norm :: Number -> Number -> Number
    norm x y = sqrt $ (+) (x `pow` 2.0) (y `pow` 2.0)
