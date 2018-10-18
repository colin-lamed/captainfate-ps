module Motor.Interpreter.ExitsInterpreter
  ( buildExits
  ) where

import Prelude
import Control.Comonad.Cofree.Trans (CofreeT, coiterT)
import Control.Comonad.Store (Store, store)
import Control.Comonad.Traced (class ComonadTraced, TracedT(TracedT), track)
import Control.Extend (duplicate)
import Control.Monad.State (class MonadState, state)
import Data.Functor.Pairing (type (⋈))
import Data.Functor.Pairing.PairEffect (pairEffect)
import Data.Functor.Product.Infix((*:*), (>:<))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

import Motor.Story.Types
import Motor.Interpreter.StateInterpreter(coGetState)


coAddExit
  ∷ ∀ w a
  . ComonadTraced (Array Exit) w
  ⇒ w a
  → CoAddExitF (w a)
coAddExit w = CoAddExit $ \exit →
  track [exit] (duplicate w)


type Stack                     = TracedT (Array Exit) (Store Story)
type ExitsBuilderInterpreter a = CofreeT CoExitsBuilderF Stack a

mkCofree
  ∷ ∀ a
  . Stack a
  → ExitsBuilderInterpreter a
mkCofree =
  coiterT (coAddExit *:* coGetState)

interpret
  ∷ ∀ a r c
  . (a → r → c)
  → ExitsBuilderInterpreter a
  → ExitsBuilder r
  → c
interpret f interpreter =
    unwrap <<< pairEffect exitsBuilderPairing f interpreter

exitsBuilderPairing ∷ CoExitsBuilderF ⋈ ExitsBuilderF
exitsBuilderPairing =   addExitPairing
                    >:< getStatePairing


buildExits
  ∷ ∀ m r
  . MonadState Story m
  ⇒ ExitsBuilder r
  → m (Array Exit)
buildExits exitsBuilder =
  state $ \s →
    let start ∷ Stack (Tuple (Array Exit) Story)
        start = TracedT $ store (\s' es → Tuple es s') s
        interpreter = mkCofree start
        Tuple exits _ = interpret (\l _ → l) interpreter exitsBuilder
    in Tuple exits s
