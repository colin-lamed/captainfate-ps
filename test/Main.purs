module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Story.CaptainFateSpec as CaptainFateSpec
import Test.Assert (ASSERT)

main ∷ ∀ e. Eff (console ∷ CONSOLE, assert ∷ ASSERT | e) Unit
main = do
  CaptainFateSpec.spec
