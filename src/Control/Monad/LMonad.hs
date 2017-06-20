{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.LMonad (
  LMonad,
  (>>=), (>>),
  return, fail
) where

import Data.Functor.LFunctor (LFunctor())
import GHC.Base (String())

infixl 1 >>, >>=

class LFunctor m => LMonad m where
  (>>=) :: m a ⊸ (a ⊸ m b) ⊸ m b
    
  (>>) :: m () ⊸ m b ⊸ m b
  m >> k = m >>= \() -> k
  {-# INLINE (>>) #-}

  return :: a ⊸ m a

  fail :: String -> m a
