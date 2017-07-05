{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Applicative.LApplicative (
  LApplicative(..),
  (<**>),
  liftA,
  liftA2,
  liftA3,
  forever
) where

import Data.Functor.LFunctor (LFunctor(), (<$>), (<$))
import Data.Linear (($), id, liftUnit, flip)

class LFunctor f => LApplicative f where
  pure :: a ⊸ f a

  (<*>) :: f (a ⊸ b) ⊸ f a ⊸ f b

  (*>) :: f () ⊸ f b ⊸ f b
  a1 *> a2 = (id <$ a1) <*> a2

  (<*) :: f a ⊸ f () ⊸ f a
  (<*) = liftA2 liftUnit

infixl 4 <*>, <*, *>, <**>

(<**>) :: LApplicative f => f a ⊸ f (a ⊸ b) ⊸ f b
(<**>) = liftA2 (flip ($))

liftA :: LApplicative f => (a ⊸ b) ⊸ f a ⊸ f b
liftA f a = pure f <*> a

liftA2 :: LApplicative f => (a ⊸ b ⊸ c) ⊸ f a ⊸ f b ⊸ f c
liftA2 f a b = f <$> a <*> b

liftA3 :: LApplicative f => (a ⊸ b ⊸ c ⊸ d) ⊸ f a ⊸ f b ⊸ f c ⊸ f d
liftA3 f a b c = f <$> a <*> b <*> c

{-# INLINEABLE liftA #-}
{-# INLINEABLE liftA2 #-}
{-# INLINEABLE liftA3 #-}

forever :: LApplicative f => f () -> f a
forever a = let a' = a *> a' in a'
{-# INLINE forever #-}
