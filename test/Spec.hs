{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
module Main where

import Streaming.Prelude
import Prelude hiding (filter, map, drop)
import qualified Control.Monad.LMonad as L
import Test.HUnit
import System.IO
import Control.Monad.Trans
import System.Exit
import Data.Function ((&))
import Data.Functor.Identity

main :: IO ()
main = do
  results <- runTestTT tests
  case errors results + failures results == 0 of
    True  -> exitSuccess
    False -> exitWith $ ExitFailure 1

tests = TestList [TestLabel "basic stream arithmetics" stream_basic]

-- Basic arithmetics on streamed integers
stream_basic = 
  let xs :: [Integer] = concat . (toList_ :: _ ⊸ Identity [Integer]) $ 
                     each [1..100]
                   & filter even
                   & map (*2)
                   & drop 10
                   & filter (\x -> x `mod` 2 == 0)
  in TestCase $ do
    assertEqual "Length correct, " 40 (Prelude.length xs)
    assertEqual "Sum is correct, " 4880 (Prelude.sum xs)


-- Safe implementation of dangerous file interactions, as
-- explained here:
-- https://m0ar.github.io/safe-streaming/2017/06/19/linear-types-101.html
stream_file = do
  Right (a, s') <- next lineStream
  Right (b, s'') <- next lineStream
  Left () <- next lineStream
  return ()
  where
    lineStream :: Stream (LOf (IO String)) IO ()
    lineStream = lift $ openFile "stream_test_file" ReadMode L.>>= \h ->
      yield $ hGetLine h L.>>
      yield $ hGetLine h L.>>
      lift $ hClose h
