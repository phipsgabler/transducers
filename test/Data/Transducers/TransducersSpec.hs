{-# LANGUAGE Arrows #-}

module Data.Transducers.TransducersSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Arrow

import Data.Transducers

-- LIST TESTS
list_test1 = toList ((arr (+1) >>> arr show) &&& arr show) [1..5]

list_test2 = (`toList` [1..5]) $ proc x -> do
  y <- arr show <<< arr (+1) -< x
  z <- arr show -< x
  returnA -< y ++ z

list_test3 = (`toList` [1..5]) $ proc x -> do
  y <- if even x
       then returnA -< x `div` 2
       else returnA -< x * 10
  filtering (<20) -< y

list_test4 = toList (flatmapping (\x -> [x, x])) [1..5]

list_test5 = toList (proc x -> if even x then dropping -< x else returnA -< x) [1..10]


-- TREE TESTS
data Tree a = Leaf | Branch a (Tree a) (Tree a)

-- foldTree :: (a -> r -> r) -> r -> Tree a -> r
-- foldTree _ init Leaf = init
-- foldTree f init (Branch a l r) = f a (foldTree f init l) $ f (foldTree f init r)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Transducers" $ do
    it "should work" $ do
      pending
