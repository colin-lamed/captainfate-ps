module Pairing where

import Prelude
import Control.Comonad (class Comonad, extract)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple, uncurry)

import Proact.Comonad.Trans.Cofree (CofreeT, runCofreeT, CofreeF(CofreeF))
import Proact.Comonad.Class.ComonadCofree (peel)
import Proact.Monad.Trans.Free (FreeT, runFreeT, FreeF (Pure, Free))


-- new typeclass: Pairing
class (Functor f, Functor g) <= Pairing f g where
  pair ∷ ∀ a b r . (a → b → r) → f a → g b → r

--TODO create class alias ⋈ for Pairing
--infixr 8 class Pairing as ⋈

-- and some instances
instance identityPairing ∷ Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance tuplePairing1 ∷ Pairing ((→) a) (Tuple a) where
  pair p f = uncurry (p <<< f)

instance tuplePairing2 ∷ Pairing (Tuple a) ((→) a) where
  pair p f g = pair (flip p) g f

-- now we can create Pairing between Cofree f and Free g

instance cofreeFreePairing ∷ (Functor f, Functor g, Pairing f g) ⇒ Pairing (CofreeT f Identity) (FreeT g Identity) where
  pair p c f  = z a b
    where z (CofreeF a _ ) (Pure x)  = p a x
          z (CofreeF _ fs) (Free gs) = pair (pair p) (fs unit) (gs unit)
          a = unwrap $ runCofreeT c
          b = unwrap $ runFreeT f


pairEffect
  ∷ ∀ f g w m a b r
  . Pairing f g
  ⇒ Comonad w
  ⇒ Monad m
  ⇒ Functor f
  ⇒ (a → b → r)
  → CofreeT f w a
  → FreeT g m b
  → m r
pairEffect p s c = do
  mb ← runFreeT c
  case mb of
    Pure x  → pure $ p (extract s) x
    Free gs → pair (pairEffect p) (peel s) (gs unit)
