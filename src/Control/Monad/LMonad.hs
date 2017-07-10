{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}

module Control.Monad.LMonad (
  LMonad,
  (>>=), (>>),
  return, fail,
  join
) where

import Control.Applicative.LApplicative (LApplicative(), pure)
import GHC.Base (String())
import Data.Linear (id)

infixl 1 >>, >>=

class LApplicative m => LMonad m where
  (>>=) :: m a ⊸ (a ⊸ m b) ⊸ m b

  (>>) :: m () ⊸ m a ⊸ m a
  m >> k = m >>= \() -> k
  {-# INLINE (>>) #-}

  return :: a ⊸ m a
  return = pure

  fail :: String -> m a

join :: LMonad m => m (m a) ⊸ m a
join x = x >>= id

ap :: LMonad m => m (a ⊸ b) ⊸ m a ⊸ m b
ap m1 m2 = do
  x1 <- m1
  x2 <- m2
  return (x1 x2)

instance LMonad [] where
  xs >>= f = [y | x <- xs, y <- f x]

  fail _ = []
