{-# LANGUAGE RankNTypes #-}

module Data.Transducers (
  Transducer
  ) where

newtype Transducer a b = Transducer (forall r. (r -> a -> r) -> (r -> b -> r))
