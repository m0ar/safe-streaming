{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A linear version of a Functor
module Data.Functor.LFunctor where

import Data.Linear (flip, liftUnit, ($))
import Prelude ((.))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Sum (Sum(..))
import Data.Functor.Identity (Identity(..))

class LFunctor f where
  fmap :: (a ⊸ b) ⊸ f a ⊸ f b

  (<$) :: a ⊸ f () ⊸ f a
  (<$) = fmap . liftUnit

(<$>) :: LFunctor f => (a ⊸ b) ⊸ f a ⊸ f b
(<$>) = fmap

($>) :: LFunctor f => f () ⊸ a ⊸ f a
($>) = flip (<$)
 
infixl 4 $>
infixl 4 <$
infixl 4 <$>

instance (LFunctor f, LFunctor g) => LFunctor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (LFunctor f, LFunctor g) => LFunctor (Sum f g) where
  fmap f (InL x) = InL $ fmap f x
  fmap f (InR y) = InR $ fmap f y

instance LFunctor Identity where
  fmap f (Identity a) = Identity $ f a
