module Motor.Interpreter.StateInterpreter
  ( coMkState
  , coGetState
  , coSetState
  ) where

import Prelude
import Control.Comonad.Store (class ComonadStore, pos, seeks)
import Data.Exists (Exists, mkExists)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)

import Motor.Story.Lens
import Motor.Story.Types


coMkState
  ∷ ∀ w a
  . ComonadStore Story w
  ⇒ w a
  → CoMkStateF (w a)
coMkState w =
  let exists = mkExists $ CoMkStateF1 { next : \l val toDyn →
        let sid = l -- we could generate label instead of client passing in (e.g. UUID)
        in Tuple (Sid sid) (seeks (sStates %~ (M.insert sid (toDyn val))) w)
      }
  in CoMkState exists

coGetState
  ∷ ∀ w a
  . ComonadStore Story w
  ⇒ w a
  → CoGetStateF (w a)
coGetState w =
  let exists = mkExists $ CoGetStateF1 { next : \(Sid sid) fromDyn →
        let s   = pos w
            val ∷ a
            val = case M.lookup sid (s ^. sStates) of
                    Just dyn → case fromDyn dyn of
                                  Just val' → val'
                                  Nothing   → unsafeCrashWith ("could not extract value from" <> show sid)
                    Nothing → unsafeCrashWith ("State " <> show sid <> " has not been added to story")
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
      exists = mkExists $ CoSetStateF1  { next : \(Sid sid) val toDyn →
                 seeks (sStates %~ (M.insert sid (toDyn val))) w
               }
  in CoSetState exists
