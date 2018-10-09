module Data.Functor.Pairing.PairEffect where

import Prelude
import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree.Trans (CofreeT)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Free.Trans (FreeT, resume)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct(..))
import Data.Functor.Pairing (type (⋈), Pairing, sym, zap)
import Data.Functor.Product (Product(..))
import Data.Tuple (Tuple(..), uncurry)


pairEffect
  ∷ ∀ f g w m a b r
  . Comonad w
  ⇒ Monad m
  ⇒ MonadRec m
  ⇒ Functor f
  ⇒ Functor g
  ⇒ f ⋈ g
  → (a → b → r)
  → CofreeT f w a
  → FreeT g m b
  → m r
pairEffect pairing p s c = do
  mb <- resume c
  case mb of
    Left x   → pure $ p (extract s) x
    Right gs → pairing (pairEffect pairing p) (unwrapCofree s) gs


{-
pairEffectM
  ∷ ∀ f g w m a b r
  . PairingM f g m
  ⇒ Comonad w
  ⇒ Monad m
  ⇒ Functor f
  ⇒ (a → b → m r)
  → CofreeT f w a
  → FreeT g m b
  → m r
pairEffectM p s c = do
  mb <- resume c
  case mb of
    Left x  → p (extract s) x
    Right gs → pairM (pairEffectM p) (unwrapCofree s) gs
-}


-- TODO why not in purescript-pairing?
funcTuplePairing ∷ ∀ a. ((→) a) ⋈ (Tuple a)
funcTuplePairing f ga (Tuple d b) = f (ga d) b

tupleFuncPairing ∷ ∀ a. (Tuple a) ⋈ ((→) a)
tupleFuncPairing = sym funcTuplePairing
-- tupleFuncPairing f (Tuple a b) ga = f b (ga a)

uncurry2 :: forall a b c. (a -> b -> c) -> Tuple a b -> c
uncurry2 = zap funcTuplePairing
