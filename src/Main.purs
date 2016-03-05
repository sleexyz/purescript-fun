module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Prelude
import Data.Monoid
import Data.Monoid.Additive



main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
