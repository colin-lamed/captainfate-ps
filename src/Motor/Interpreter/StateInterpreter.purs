module Motor.Interpreter.StateInterpreter
  ( interpretGetState
  ) where

import Prelude
import Control.Monad (class Monad)
import Control.Monad.Free (Free, runFreeM)
import Control.Monad.State (StateT, State, modify, execStateT, get, put, runStateT)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.Exists (Exists, runExists)
import Data.Map as M
import Data.List as L
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Motor.Story


interpretGetState ∷ ∀ next m. Monad m => Exists (GetStateF next) → StateT Story m next
interpretGetState exists = do
  story ← get
  let a = runExists (\(GetStateF { sid: (Sid sid), fromDyn, next }) →
                      case M.lookup sid story.states of
                        Just dyn → case fromDyn dyn of
                          Just val' → next val'
                          Nothing   → unsafeCrashWith ("could not extract value from " <> show sid)
                        Nothing → unsafeCrashWith ("State " <> show sid <> "has not been added to story")
                    ) exists
  pure a
