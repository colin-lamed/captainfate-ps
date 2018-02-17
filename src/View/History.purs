module View.History
  ( readPath
  , writePath
  , updateHistory
  ) where

import Prelude
import Browser.WebStorage (WEB_STORAGE)
import Browser.WebStorage as WS
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Motor.History as H
import View.Types (SS)

readPath
  ∷ ∀ eff
  . Eff ( webStorage ∷ WEB_STORAGE | eff) String
readPath = do
  mPath ← WS.getItem WS.localStorage "path"
  pure (maybe "" id mPath)

writePath
  ∷ ∀ eff
  . String
  → Eff ( webStorage ∷ WEB_STORAGE | eff) Unit
writePath path =
  WS.setItem WS.localStorage "path" path

updateHistory
  ∷ (H.History → H.History)
  → SS Unit
updateHistory f = do
  oldPath ← lift $ lift readPath
  case H.deserialse oldPath of
    Right h → lift $ lift $ writePath $ H.serialise $ f h
    Left h  → lift $ lift $ log ("Failed to parse history: " <> h)
