module Data.Transducers.TransducersSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Transducers

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Transducers" $ do
    it "should work" $ do
      pending
