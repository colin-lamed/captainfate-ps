module View.Browser.Utils where

import Control.Monad.Eff (kind Effect, Eff)
import DOM (DOM())

foreign import getOffsetHeight ∷ ∀ eff. String → Eff (dom ∷ DOM | eff) Number
