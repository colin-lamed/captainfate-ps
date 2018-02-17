module View.Utils where

import Control.Monad.Eff (kind Effect, Eff)
import Data.Foreign (Foreign)
import DOM (DOM())

foreign import getOffsetHeight :: ∀ eff. String → Eff (dom :: DOM | eff) Number
