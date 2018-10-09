-- http://reasonablypolymorphic.com/blog/better-data-types-a-la-carte
module Data.Functor.Product.Infix where

import Prelude
import Control.Apply (lift2)
import Data.Either (Either(Left,Right))
import Data.Functor.Pairing (productCoproduct)
import Data.Functor.Product (Product(Product), product)
import Data.Functor.Coproduct (Coproduct(Coproduct))
import Data.Tuple(Tuple(Tuple))


infixr 5 type Coproduct as :+:
infixr 5 type Coproduct as ⊕
infixr 6 type Product as :*:
infixr 6 type Product as ⊗
infixr 6 Product as ⊗

lift2Product ∷ ∀ a f g. (a → f a) → (a → g a) → a → (Product f g) a
lift2Product = lift2 product
infixr 9 lift2Product as *:*


infixr 9 productCoproduct as >:<
