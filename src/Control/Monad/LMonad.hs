{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.LMonad (
  LMonad,
  (>>=), (>>),
  return, fail,
  join, ap
) where

import Control.Applicative.LApplicative (LApplicative(..), pure)
import Data.Functor.LFunctor
import Prelude (error)
import GHC.Base (String(), bindIO, returnIO)
import GHC.IO (failIO)
import Data.Linear (id, (.))
import System.IO (IO)
import Data.Functor.Identity
import Unsafe.Coerce

infixl 1 >>, >>=

class LApplicative m => LMonad m where
  (>>=) :: m a ⊸ (a ⊸ m b) ⊸ m b

  (>>) :: m () ⊸ m a ⊸ m a
  m >> k = m >>= \() -> k
  {-# INLINE (>>) #-}

  return :: a ⊸ m a
  return = pure

  fail :: String -> m a
  fail = error

join :: LMonad m => m (m a) ⊸ m a
join x = x >>= id

ap :: LMonad m => m (a ⊸ b) ⊸ m a ⊸ m b
ap m1 m2 = do
  x1 <- m1
  x2 <- m2
  return (x1 x2)

instance LMonad Identity where
  (Identity a) >>= k = k a


-- LFunctor & LApplicative IO defined here to escape recursive imports
instance LFunctor IO where
  fmap f x = x >>= return . f

instance LApplicative IO where
  pure  = return
  (<*>) = ap

-- All of the IO monad functions are already linear, so unsafeCoerce is fine
instance LMonad IO where
  {-# INLINE (>>=) #-}
  return = unsafeCoerce returnIO
  (>>=) = unsafeCoerce bindIO
  fail  = unsafeCoerce failIO

