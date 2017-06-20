{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A linear version of a Functor
module Data.Functor.LFunctor where

import Data.Linear (flip)

class LFunctor f where
  fmap :: (a ⊸ b) -> f a ⊸ f b
  (<$) :: a -> f () ⊸ f a

(<$>) :: LFunctor f => (a ⊸ b) -> f a ⊸ f b
(<$>) = fmap

($>) :: LFunctor f => f () ⊸ a -> f a
($>) = flip (<$)
 
infixl 4 $>
infixl 4 <$
infixl 4 <$>


