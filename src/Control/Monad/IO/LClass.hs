{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.IO.LClass where

import Control.Monad.LMonad (LMonad())
import System.IO (IO())
import Data.Linear (id)

class LMonad m => LMonadIO m where
  liftIO :: IO a -> m a

instance LMonadIO IO where
  liftIO = id
