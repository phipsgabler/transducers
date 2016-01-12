{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Transducers.TransducersSpec  where

import Test.Hspec
import Test.QuickCheck
import Control.Arrow

import Data.Transducers
import Data.Transducers.Transducer

-- LIST TESTS
list_test1 = value `shouldBe` result where
  value =  toList ((arr (+1) >>> arr show) &&& arr show) [1..5]
  result =  [("2","1"),("3","2"),("4","3"),("5","4"),("6","5")]

list_test2 = value `shouldBe` result where
  value = (`toList` [1..5]) $ proc x -> do
    y <- arr show <<< arr (+1) -< x
    z <- arr show -< x
    returnA -< y ++ z
  result = ["21","32","43","54","65"]

list_test3 = value `shouldBe` result where
  value = (`toList` [1..5]) $ proc x -> do
    y <- if even x
         then returnA -< x `div` 2
         else returnA -< x * 10
    filtering (<20) -< y
  result = filter (<20) [10, 1, 30, 2, 50] 

list_test4 = toList (flatMapping (\x -> [x, x])) [1..5] `shouldBe` ([1..5] >>= (\x -> [x, x]))

list_test5 = value `shouldBe` result where
  value = (`toList` [1..10]) $ proc x -> if even x
                                         then dropping -< x
                                         else returnA -< x
  result = filter (not . even) [1..10]


-- TREE TESTS
data Tree a = Leaf | Branch a (Tree a) (Tree a)

-- foldTree :: (a -> r -> r) -> r -> Tree a -> r
-- foldTree _ init Leaf = init
-- foldTree f init (Branch a l r) = f a (foldTree f init l) $ f (foldTree f init r)

-- BITS TEST
-- data Bit = I | O deriving (Eq, Show)
-- newtype Bits = Bits [Bit] deriving (Eq, Show)

-- invert :: Bit -> Bit
-- invert I = O
-- invert O = I

-- xor :: Bit -> Bit -> Bit
-- xor a b | (a == b) = O
--         | otherwise = I

-- instance MonoTransducible Bits where
--   type Elem Bits = Bit
--   monoTransduce t f z (Bits bs) = transduce t f z bs

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Transducers" $ do
    it "allow basic arrow operations" $ list_test1
    it "work with proc notation" $ list_test2
    it "support filtering" $ list_test3
    it "support flatmapping" $ list_test4
    it "support dropping" $ list_test5
    
