module Utils where

import Test.QuickCheck

sample_ :: Show a => Gen a -> IO ()
sample_ gen_a = sample_' 10
  where
  sample_' :: Int -> IO ()
  sample_' n 
    | n <= 0 = return ()
    | otherwise = do
        a <- generate gen_a
        print a
        sample_' (n-1)

data MyType = MyType {
    foo :: Int
  , bar :: Bool
  , baz :: Char
} deriving (Show)
