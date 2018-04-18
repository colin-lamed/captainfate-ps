module ProductUtils where

import Prelude
import Control.Comonad.Traced       (class ComonadTraced, track)
import Control.Extend (duplicate)


-- | Add m to w a
tell ∷
  ∀ m w a
  . ComonadTraced m w
  ⇒ m
  → w a
  → w a
tell m w =
  track m (duplicate w)
