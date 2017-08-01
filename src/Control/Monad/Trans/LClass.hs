{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Trans.LClass (
  LMonadTrans(..)
) where

import Control.Monad.LMonad (LMonad())

class LMonadTrans t where
  lift :: (LMonad m) => m a ⊸ t m a
