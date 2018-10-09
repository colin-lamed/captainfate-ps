module Motor.Interpreter.UseActionInterpreter
  ( runUseAction
  ) where

import Prelude
import Control.Comonad.Cofree.Trans (CofreeT, coiterT)
import Control.Comonad.Store (Store, store)
import Control.Comonad.Traced (class ComonadTraced, TracedT(TracedT), track)
import Control.Extend (duplicate)
import Control.Monad.State (class MonadState, state)
import Data.Functor.Coproduct (Coproduct(..))
import Data.Functor.Pairing (type (⋈))
import Data.Functor.Pairing.PairEffect (pairEffect)
import Data.Functor.Product (Product(..))
import Data.Functor.Product.Infix ((*:*), (>:<))
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(First))
import Data.Tuple (Tuple(..))

import Motor.Story.Types
import Motor.Interpreter.StateInterpreter (coGetState)


coWith
  ∷ ∀ w a
  . ComonadTraced (First (Action Unit)) w
  ⇒ Oid
  → w a
  → CoWithF (w a)
coWith oid w = CoWith $ \oid' a →
  if oid == oid'
    then track (First $ Just a) (duplicate w)
    else w

-- Note, First means we ignore any others if there's more than one
type Stack                  = TracedT (First (Action Unit)) (Store Story)
type UseActionInterpreter a = CofreeT CoUseActionF Stack a

mkCofree
  ∷ ∀ a
  . Oid
  → Stack a
  → UseActionInterpreter a
mkCofree oid ts =
  coiterT (   (coWith oid)
          *:* coGetState
          ) ts

interpret
  ∷ ∀ a r c
  . (a → r → c)
  → UseActionInterpreter a
  → UseAction r
  → c
interpret f interpreter =
    unwrap <<< pairEffect userActionPairing f interpreter

userActionPairing ∷ CoUseActionF ⋈ UseActionF
userActionPairing =   withPairing
                  >:< getStatePairing


runUseAction
  ∷ ∀ m r
  . MonadState Story m
  ⇒ UseAction r
  → Oid
  → m (Maybe (Action Unit))
runUseAction useAction oid =
  state $ \s →
    let start ∷ Stack (First (Tuple (Action Unit) Story))
        start = TracedT $ store (\s' fma' →
                   case unwrap fma' of
                     Nothing → First Nothing
                     Just ma' → First (Just $ Tuple ma' s')
                 ) s -- store ∷ (Story → (First (Action Unit) → First (Tuple (Action Unit) Story))) → Story → Store Story (First (Action Unit) → First (Tuple (Action Unit) Story))
        interpreter = mkCofree oid start
        x ∷ Maybe (Tuple (Action Unit) Story)
        x = unwrap $ interpret (\l _ → l) interpreter useAction
    in case x of
         Nothing            → Tuple Nothing s
         Just (Tuple ma s') → Tuple (Just ma) s'
