{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module FileMock where

import Control.Monad.State.LState
import Control.Monad.LMonad
import Data.Linear
import Prelude hiding ((>>=), (>>), return, fail, ($), (.))


data Handle where Handle :: Int -> Handle
type Path = String
type FileIO = LState (S [String])

openFile :: Path -> Handle
openFile _ = Handle 0

closeFile :: Handle ⊸ ()
closeFile (Handle _) = ()

getLine :: Handle ⊸ FileIO (String, Handle)
getLine (Handle i) = do
  ss <- get
  if i > length ss then fail "EOF"
                   else return (ss !! i, Handle (i+1))
