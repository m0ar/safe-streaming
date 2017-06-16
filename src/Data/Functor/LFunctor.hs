{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A linear version of a Functor
module Data.Functor.LFunctor where

import Prelude (flip)

class LFunctor f where
    fmap :: (a ⊸ b) ⊸ f a ⊸ f b
    (<$) :: a -> f b ⊸ f a

infixl 4 <$>

(<$>) :: LFunctor f => (a ⊸ b) ⊸ f a ⊸ f b
(<$>) = fmap

infixl 4 $>

($>) :: LFunctor f => f a ⊸ b -> f b
($>) = flip (<$)

void :: LFunctor f => f a -> f ()
void x = () <$ x
