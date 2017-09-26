{-# LANGUAGE ScopedTypeVariables #-}
module Spec where

import Test.Tasty(defaultMain, TestTree, testGroup)
-- import Test.Tasty.QuickCheck (testProperty, Gen, reason, arbitrary)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Gen, arbitrary, quickCheck, choose)
import Test.QuickCheck.Property (Result, failed, succeeded)
-- import Test.QuickCheck.Test (Result)

-- QuickCheck property
prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative a b = a + b == b + a

tests :: TestTree
tests = testProperty "Addition is commutative" prop_additionCommutative

-- The following two instance definitions make the 'Gen Result' a testable
-- instance [safe] Testable Test.QuickCheck.Property.Result
-- instance [safe] Testable prop => Testable (Gen prop)
prop_commutativeAdd :: Gen Result
prop_commutativeAdd = do
  (x, y) <- arbitrary :: Gen (Int, Int)
  return $ if x + y == y + x
    then succeeded
    else failed 

-- The Following instance definition makes 'the function body' as testable
-- instance [safe] (Arbitrary a, Show a, Testable prop) => Testable (a -> prop)
-- Bool is Testable (instance [safe] Testable Bool) as defined
-- therefore (Int -> Bool) is also testable
-- therefore (Int -> (Int -> Bool)) is also testable !!
-- The last argument should be a testable
prop_commutativeAdd' :: Int -> Int -> Bool
prop_commutativeAdd' x y = x + y == y + x

prop_commutativeAdd2 :: Int -> Int -> Result
prop_commutativeAdd2 x y = 
  if x + y == y + x
  then succeeded
  else failed

-- instance [safe] Testable Bool
prop_boooool5 :: Bool
prop_boooool5 = True

-- instance [safe] Testable Test.QuickCheck.Property.Result
prop_boooool4 :: Result
prop_boooool4 = succeeded

-- instance [safe] Testable prop => Testable (Gen prop)
prop_boooool6 :: Gen Result
prop_boooool6 = (return succeeded) :: Gen Result

-- instance [safe] (Arbitrary a, Show a, Testable prop) => Testable (a -> prop)
prop_boooool :: Bool -> Bool
prop_boooool b = True

-- instance [safe] (Arbitrary a, Show a, Testable prop) => Testable (a -> prop)
prop_boooool1 :: Bool -> Result
prop_boooool1 b = succeeded

-- instance [safe] (Arbitrary a, Show a, Testable prop) => Testable (a -> prop)
prop_boooool2 :: Bool -> Gen Bool
prop_boooool2 b = do
  b' <- choose (True, True)
  return b'

-- instance [safe] (Arbitrary a, Show a, Testable prop) => Testable (a -> prop)
prop_boooool3 :: Bool -> Gen Result
prop_boooool3 b = do
  b' <- choose (True, True)
  return succeeded

main :: IO ()
main = do
  quickCheck prop_commutativeAdd
  quickCheck prop_commutativeAdd'
  quickCheck prop_commutativeAdd2
  quickCheck prop_boooool
  quickCheck prop_boooool1
  quickCheck prop_boooool2
  quickCheck prop_boooool3
  quickCheck prop_boooool4
  quickCheck prop_boooool5
  quickCheck prop_boooool6
  -- defaultMain $ testProperty "prop_commutativeAdd" prop_commutativeAdd
  defaultMain $ testGroup "different kinds of properties" 
    [testProperty "prop_commutativeAdd'" prop_commutativeAdd'
    , testProperty "prop_commutativeAdd2" prop_commutativeAdd2
    , testProperty "prop_commutativeAdd" prop_commutativeAdd
    , testProperty "Addition is commutative" prop_additionCommutative]
