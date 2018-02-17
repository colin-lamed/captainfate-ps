module Main where

import Prelude
import Browser.WebStorage (WEB_STORAGE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Story.CaptainFate as CaptainFate
import View.View (view)

main ∷ Eff ( console    ∷ CONSOLE
           , dom        ∷ DOM
           , webStorage ∷ WEB_STORAGE
           ) Unit
main =
  view CaptainFate.story
