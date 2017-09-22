{-# LANGUAGE ScopedTypeVariables #-}
module Spec where

import Test.Tasty(defaultMain, TestTree)
import Test.Tasty.QuickCheck (testProperty)

-- QuickCheck property
prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative a b = a + b == b + a

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testProperty "Addition is commutative" prop_additionCommutative

