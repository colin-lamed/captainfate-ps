module Motor.Interpreter.StateInterpreter
  ( coGetState
  , coSetState
  ) where

import Prelude
import Control.Comonad.Store (class ComonadStore, pos, seeks)
import Data.Exists (Exists, mkExists)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign (unsafeFromForeign, unsafeToForeign)
import Partial.Unsafe (unsafeCrashWith)

import Motor.Story.Lens (sStates, (%~), (^.))
import Motor.Story.Types


coGetState
  ∷ ∀ w a
  . ComonadStore Story w
  ⇒ w a
  → CoGetStateF (w a)
coGetState w =
  let exists = mkExists $ CoGetStateF1 { next : \(Sid sid) →
        let s   = pos w
            val ∷ a
            val = case M.lookup sid (s ^. sStates) of
                    Just dyn → unsafeFromForeign dyn
                    Nothing  → unsafeCrashWith ("State " <> show sid <> " has not been added to story")
        in Tuple val w
      }
  in CoGetState exists

coSetState
  ∷ ∀ w a
  . ComonadStore Story w
  ⇒ w a
  → CoSetStateF (w a)
coSetState w =
  let exists ∷ (Exists (CoSetStateF1 (w a)))
      exists = mkExists $ CoSetStateF1  { next : \(Sid sid) val →
                 seeks (sStates %~ (M.insert sid (unsafeToForeign val))) w
               }
  in CoSetState exists
