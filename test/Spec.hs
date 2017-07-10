{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures #-}
module Main where

import Streaming.Prelude
import Prelude hiding (filter, map, drop)
import Test.HUnit
import System.Exit
import Data.Function ((&))

main :: IO ()
main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)

tests = TestList [TestLabel "basic stream arithmetics" stream_basic]

stream_basic = 
  let xs :: [Integer] = concat . (toList_ :: _ ⊸ [[Integer]]) $ 
                     each [1..100]
                   & filter even
                   & map (*2)
                   & drop 10
                   & filter (\x -> x `mod` 2 == 0)
  in TestCase $ do
    assertEqual "Length correct, " 40 (Prelude.length xs)
    assertEqual "Sum is correct, " 4880 (Prelude.sum xs)
