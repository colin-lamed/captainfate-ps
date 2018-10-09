module Main where

import Prelude
import Effect (Effect)
import Motor.View (commandLineView)
import Story.CaptainFate as CaptainFate

main ∷ Effect Unit
main =
  commandLineView CaptainFate.story
