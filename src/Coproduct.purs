-- http://reasonablypolymorphic.com/blog/better-data-types-a-la-carte
module Coproduct where

import Prelude
import Pairing (class Pairing, pair)
import Control.Apply (lift2)

data Coproduct f g a
  = InL (f a)
  | InR (g a)
derive instance coproductFunctor ∷ (Functor f, Functor g) ⇒ Functor (Coproduct f g)


infixr 5 type Coproduct as :+:
infixr 5 type Coproduct as ⊕


class (Functor sub, Functor sup) <= Inj sub sup where
  inj ∷ ∀ a. sub a → sup a

--infixr 8 class Inj as :<:

instance functorInj ∷ (Functor f) ⇒ Inj f f where
  inj = id

instance functorCoprojectInj ∷ (Functor f, Functor g) ⇒ Inj f (Coproduct f g) where
  inj = InL

-- {-# OVERLAPPABLE #-}
instance functorCoprojectInj2 ∷  (Functor f, Functor g, Functor h, Inj f g) ⇒ Inj f (Coproduct h g) where
  inj = InR <<< inj


data Product f g a
  = Product (f a) (g a)
derive instance productFunctor ∷ (Functor f, Functor g) ⇒ Functor (Product f g)

infixr 6 type Product as :*:
infixr 6 type Product as ⊗
infixr 6 Product as ⊗


instance coproductProductPairing ∷ (Functor f, Functor g, Functor f', Functor f', Pairing f f', Pairing g g') ⇒ Pairing (Coproduct f g) (Product f' g') where
  pair p (InL x) (Product a _) = pair p x a
  pair p (InR x) (Product _ b) = pair p x b

instance productCoproductPairing ∷ (Functor f, Functor g, Functor f', Functor f', Pairing f f', Pairing g g') ⇒ Pairing (Product f g) (Coproduct f' g') where
  pair p (Product a _) (InL x) = pair p a x
  pair p (Product _ b) (InR x) = pair p b x

lift2Product ∷ ∀ a f g. (a → f a) → (a → g a) → a → (Product f g) a
lift2Product = lift2 Product
infixr 9 lift2Product as *:*
