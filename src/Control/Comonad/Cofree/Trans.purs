module Control.Comonad.Cofree.Trans
  ( module Control.Comonad.Cofree.Class
  , CofreeF(..)
  , CofreeT(..)
  , coiterT
  , runCofreeT
  )
where

import Control.Comonad (class Comonad, class Extend, extend, extract)
import Control.Comonad.Cofree.Class (class ComonadCofree)
import Control.Comonad.Trans.Class (class ComonadTrans)
import Data.Bifunctor (class Bifunctor, bimap, rmap)
import Data.Lazy (Lazy, defer, force)
import Data.Newtype (class Newtype, unwrap)
import Prelude


data CofreeF f a b = CofreeF a (Lazy (f b))

newtype CofreeT f w a = CofreeT (w (CofreeF f a (CofreeT f w a)))

derive instance functorCofreeF :: (Functor f) => Functor (CofreeF f a)

instance bifunctorCofreeF :: (Functor f) => Bifunctor (CofreeF f) where
  bimap f g (CofreeF a fb) = CofreeF (f a) (map g <$> fb)

derive instance newtypeCofreeT :: Newtype (CofreeT f w a) _

instance functorCofreeT :: (Functor f, Functor w) => Functor (CofreeT f w) where
  map f (CofreeT w) = CofreeT $ map (bimap f (map f)) w

instance extendCofreeT :: (Functor f, Comonad w) => Extend (CofreeT f w) where
  extend f = CofreeT <<< extend extendStep <<< runCofreeT
    where
    extendStep w =
      CofreeF (f (CofreeT w)) (defer \_ -> snd $ rmap (extend f) $ extract w)

instance comonadCofreeT :: (Functor f, Comonad w) => Comonad (CofreeT f w) where
  extract (CofreeT w) = fst (extract w)

instance comonadTransCofreeT :: ComonadTrans (CofreeT f) where
  lower = map fst <<< runCofreeT

instance comonadCofreeCofreeT :: (Functor f, Comonad w) => ComonadCofree f (CofreeT f w) where
  unwrapCofree = snd <<< extract <<< runCofreeT



-- | Unfolds a `CofreeT` Comonad Transformer from a Coalgebra and an initial
-- | Comonad.
coiterT
  :: forall f w a
  . Functor f
  => Comonad w
  => (w a -> f (w a))
  -> w a
  -> CofreeT f w a
coiterT f =
  CofreeT <<< extend (\w -> CofreeF (extract w) (defer \_ ->  map (coiterT f) (f w)))

-- | Deconstructs a `CofreeT` Comonad Transformer.
runCofreeT
  :: forall f w a
  . CofreeT f w a
  -> w (CofreeF f a (CofreeT f w a))
runCofreeT = unwrap

-- Returns the first item of the `CofreeF` tuple.
fst :: forall f a b . CofreeF f a b -> a
fst (CofreeF a _) = a

-- Returns the second item of the `CofreeF` tuple.
snd :: forall f a b . CofreeF f a b -> f b
snd (CofreeF _ fb) = force fb
