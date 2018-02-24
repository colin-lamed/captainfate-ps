module Motor.Interpreter.StateInterpreter
  ( interpretGetState
  ) where

import Prelude (class Monad, bind, pure, show, (<<<), (<>))
import Control.Monad.State (StateT, get)
import Data.Exists (Exists, runExists)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import Motor.Story.Lens (at, sStates, (^.))
import Motor.Story.Types (GetStateF(..), Sid(..), Story)


interpretGetState ∷ ∀ next m. Monad m ⇒ Exists (GetStateF next) → StateT Story m next
interpretGetState exists = do
  story ← get
  let a = runExists (\(GetStateF { sid: (Sid sid), fromDyn, next }) →
                      case story ^. (sStates <<< at sid) of
                        Just dyn → case fromDyn dyn of
                          Just val' → next val'
                          Nothing   → unsafeCrashWith ("could not extract value from " <> show sid)
                        Nothing → unsafeCrashWith ("State " <> show sid <> "has not been added to story")
                    ) exists
  pure a
