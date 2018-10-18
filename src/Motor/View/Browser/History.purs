module Motor.View.Browser.History
  ( readPath
  , writePath
  , updateHistory
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Console (log)
import Motor.History as H

readPath
  ∷ Effect String
readPath = do
  mPath ← map toMaybe $ getLocalStorage "path"
  pure (maybe "" identity mPath)

writePath
  ∷ String
  → Effect Unit
writePath path =
  setLocalStorage "path" path

updateHistory
  ∷ (H.History → H.History)
  → Effect Unit
updateHistory f = do
  oldPath ← readPath
  case H.deserialse oldPath of
    Right h  → writePath $ H.serialise $ f h
    Left err → log ("Failed to parse history: " <> err)

foreign import getLocalStorage :: String -> Effect (Nullable String)

foreign import setLocalStorage :: String -> String -> Effect Unit
