{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Main where

import Streaming.Prelude
import Prelude hiding (filter, map, drop,
                       (>>=), (>>), return,
                       fail, getLine, (++), ($), id)
import qualified Prelude as P
import Control.Monad.LMonad
import Data.Linear (($), id, (++))
import Control.Monad.Trans.LClass
import Test.HUnit
import Control.Monad.State.LState
import System.Exit
import Data.Function ((&))
import Data.Functor.Identity
import FileMock

main :: IO ()
main = do
  results <- runTestTT tests
  case errors results + failures results == 0 of
    True  -> exitSuccess
    False -> exitWith $ ExitFailure 1

tests :: Test
tests = TestList [ TestLabel "basic stream arithmetics" streamBasic
                 , TestLabel "basic S state modification" stateModS
                 , TestLabel "basic PLS state modification" stateModPLS
                 , TestLabel "derpy LS state modification" stateModLS
                 , TestLabel "streaming lines from a mocked file" streamFile]

-- Basic arithmetics on streamed integers
streamBasic :: Test
streamBasic =
  let xs :: [Integer] = P.concat . (toList_ :: _ ⊸ Identity [Integer]) $
                     each [1..100]
                   & filter even
                   & map (*2)
                   & drop 10
                   & filter (\x -> x `mod` 2 == 0)
  in TestCase $
    assertEqual "Length" 40 (Prelude.length xs) P.>>
    assertEqual "Sum" 4880 (Prelude.sum xs)


-----------------------------------
-- Some testing of the LState monad
-----------------------------------
stateModS :: Test
stateModS =
  let (S x, ()) = runLState (do
                    modify $ \(S i) -> S (i*2)
                    modify $ \(S i') -> S (i'+1)
                    ) (S 5)
  in TestCase $ assertEqual "Result" 11 (x :: Int)

stateModPLS :: Test
stateModPLS =
  let (PLS _ x, ()) = runLState (do
                        modify $ \(PLS a b) -> PLS a (b*2)
                        modify $ \(PLS a' b') -> PLS a' (b'+1)
                      ) (PLS undefined 5)
  in TestCase $ assertEqual "Result" 11 (x :: Int)

-- Argh, can't use integer arithmetics on linear variables, wont take the time
-- implementing them either because of unboxed primitives etc...
stateModLS :: Test
stateModLS =
  let (LS x, ()) = runLState (do
                     modify id
                   ) (LS 5)
  in TestCase $ assertEqual "Result" 5 (x :: Int)


-- Safe implementation of dangerous file interactions, as
-- explained here:
-- https://m0ar.github.io/safe-streaming/2017/06/19/linear-types-101.html
--
-- Doesn't use do-notation for the LState, because we have stumbled upon
-- an odd bug related to the RebindableSyntax extension that confuses the
-- linearity checker. Works as intended with the infixes though! \o/
streamFile :: Test
streamFile =
  let (S _, str) = runLState (
             next lineStream >>= \(Right (a, s)) ->
             next s  >>= \(Right (b, s')) ->
             next s' >>= \(Left ()) ->
             return $ a ++ b
             ) (S ["herp", "derp", "secret"])
  in TestCase $ assertEqual "Result" "herpderp" (str :: String)
  where
    lineStream :: Stream (LOf String) FileIO ()
    lineStream = do
      let hdl = openFile "test_file.txt"
      (str, hdl') <- lift $ getLine hdl
      yield str
      (str', hdl'') <- lift $ getLine hdl'
      yield str'
      return $ closeFile hdl''
