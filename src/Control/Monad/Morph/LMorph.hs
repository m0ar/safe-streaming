{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
module Control.Monad.Morph.LMorph (
  LMFunctor(..)
) where

import Control.Monad.LMonad (LMonad())

class LMFunctor t where
  hoist :: LMonad m => (forall a . m a ⊸ n a) -> t m b ⊸ t n b

