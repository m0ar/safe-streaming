{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.LMonad (
    LMonad,
    (>>=), (>>),
    return, fail
) where

import Control.Applicative (Applicative(), pure)
import Data.Linear (const)
import GHC.Base (String())

class Applicative m => LMonad m where
    (>>=) :: m a ⊸ (a ⊸ m b) ⊸ m b
    
    (>>) :: m () ⊸ m b ⊸ m b
    m >> k = m >>= \() -> k
    {-# INLINE (>>) #-}

    return :: a ⊸ m a
    return = pure

    fail :: String -> m a
