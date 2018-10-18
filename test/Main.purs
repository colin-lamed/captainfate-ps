module Test.Main where

import Prelude
import Effect (Effect)
import Story.CaptainFateSpec as CaptainFateSpec
import Test.Unit.Main (runTest)



main ∷ Effect Unit
main = runTest do
  CaptainFateSpec.spec
