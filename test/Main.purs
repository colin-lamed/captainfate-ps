module Test.Main where

import Prelude
import Effect (Effect)
import Story.CaptainFateSpec as CaptainFateSpec

main ∷ Effect Unit
main = do
  CaptainFateSpec.spec
