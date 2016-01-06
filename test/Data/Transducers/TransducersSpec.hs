module Data.Transducers.TransducersSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

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


-- TREE TESTS
data Tree a = Leaf | Branch a (Tree a) (Tree a)

foldTree :: (a -> a -> r) -> r -> Tree a -> r
foldTree _ init Leaf = r
foldTree f init (Branch a l r) = f a $ f (foldTree f init l) (foldTree f init r)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Transducers" $ do
    it "should work" $ do
      pending
