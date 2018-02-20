module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import LineReader (READLINE)
import Story.CaptainFate as CaptainFate
import View.CommandLine.View (view)

main ∷ Eff (console ∷ CONSOLE, readline ∷ READLINE, exception ∷ EXCEPTION) Unit
main =
  view CaptainFate.story
