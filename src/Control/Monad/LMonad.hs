{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.LMonad (
  LMonad,
  (>>=), (>>),
  return, fail,
  join
) where

import Data.Functor.LFunctor (LFunctor())
import GHC.Base (String())
import Data.Linear (id)

infixl 1 >>, >>=

class LFunctor m => LMonad m where
  (>>=) :: m a ⊸ (a ⊸ m b) ⊸ m b

  (>>) :: m () ⊸ m a ⊸ m a
  m >> k = m >>= \() -> k
  {-# INLINE (>>) #-}

  return :: a ⊸ m a

  fail :: String -> m a

join :: LMonad m => m (m a) ⊸ m a
join x = x >>= id
