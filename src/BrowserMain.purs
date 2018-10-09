module BrowserMain where

import Prelude
import Effect (Effect)
import Motor.View (browserView)
import Story.CaptainFate as CaptainFate

main ∷ Effect Unit
main =
  browserView CaptainFate.story
