module Motor.Interpreter.ExitsInterpreter
  ( buildExits
  ) where

import Prelude
import Control.Comonad.Store (Store, store)
import Control.Monad.State (class MonadState, state)
import Control.Comonad.Traced (class ComonadTraced, TracedT(TracedT))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Motor.Story.Lens
import Proact.Comonad.Trans.Cofree (CofreeT, coiterT)

import Coproduct ((*:*))
import Motor.Story.Types
import Motor.Interpreter.StateInterpreter(coGetState)
import Pairing (pairEffect)
import ProductUtils (tell)


coAddExit ∷ ∀ w a
          . ComonadTraced (Array Exit) w
          ⇒ w a
          → CoAddExitF (w a)
coAddExit w = CoAddExit $ \exit →
  tell [exit] w


type Stack                     = TracedT (Array Exit) (Store Story)
type ExitsBuilderInterpreter a = CofreeT CoExitsBuilderF Stack a

mkCofree ∷ ∀ a
          . Stack a
         → ExitsBuilderInterpreter a
mkCofree =
  coiterT (coAddExit *:* coGetState)

interpret ∷ ∀ a r c
          . (a → r → c)
          → ExitsBuilderInterpreter a
          → ExitsBuilder r
          → c
interpret f interpreter =
    unwrap <<< pairEffect f interpreter

buildExits ∷ ∀ m r
           .  MonadState Story m ⇒ ExitsBuilder r → m (Array Exit)
buildExits exitsBuilder =
  state $ \s →
    let start                   ∷ Stack (Tuple (Array Exit) Story)
        start                   = TracedT $ store (\s' es → Tuple es s') s
        exitsBuilderInterpreter = mkCofree start
        Tuple exits _ = interpret (\l _ → l) exitsBuilderInterpreter exitsBuilder
    in Tuple exits s
