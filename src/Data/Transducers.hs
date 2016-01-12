{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module Data.Transducers (
  -- , Transducible(..)
  -- , MonoTransducible(..)
  -- , toList
  ) where


import Prelude hiding ((.), id, Functor(..))
import Control.Category
import Control.Arrow
import Data.Profunctor





-- class MonoTransducible t where
--   type Elem t :: *
--   monoTransduce :: (Elem t ~ a) => Transducer a b -> (b -> r -> r) -> r -> t -> r










