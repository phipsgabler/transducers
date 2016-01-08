{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Data.Transducers (
    Transducer (..)
  , mapping
  , filtering
  , flattening
  , flatMapping
  , Transducible
  , MonoTransducible
  , toList
  ) where

import Prelude hiding ((.), id, Functor(..))
import Control.Category
import Control.Arrow
import Data.Profunctor

newtype Transducer a b = Transducer (forall r. (b -> r -> r) -> (a -> r -> r))

instance Category Transducer where
  id = Transducer (\cons -> cons)
  (Transducer t1) . (Transducer t2) =  Transducer (\cons -> t2 . t1 $ cons)

instance Arrow Transducer where
  arr f = mapping f
  first (Transducer t) = Transducer (\cons -> \(b,d) r -> let cons' = t $ \c r -> cons (c, d) r
                                                          in cons' b r)

instance ArrowChoice Transducer where
  left (Transducer t) = Transducer (\cons ->
                                     \case Left b -> \r -> (t $ cons . Left) b r
                                           Right d -> \r -> cons (Right d) r)

-- is this even possible?
-- instance ArrowLoop Transducer where
--   loop (Transducer t) = Transducer (\cons -> \b r -> undefined)

-- unnecessary in theory (cf. WrappedArrow), but instructional.
instance Profunctor Transducer where
  lmap f (Transducer t) = Transducer (\cons -> \c r -> (t cons) (f c) r)
  rmap f (Transducer t) = Transducer (\cons -> \a r -> (t $ cons . f) a r )



mapping :: (a -> b) -> Transducer a b
mapping f = Transducer (\cons -> \a r -> cons (f a) r)

filtering :: (a -> Bool) -> Transducer a a
filtering p = Transducer (\cons -> \a r -> if p a then cons a r else r)

flattening :: Foldable t => Transducer (t a) a
flattening = Transducer (\cons -> \as r -> foldr cons r as)

flatMapping :: Foldable t => (a -> t b) -> Transducer a b
flatMapping f = mapping f >>> flattening

-- equivalent to `filtering (const False)`
dropping :: Transducer a a
dropping = Transducer (\cons -> \_ r -> r)


-- HOW TO IMPLEMENT THESE?
-- reversing :: Transducer a a
-- reversing = Transducer (\cons -> \a r -> _)

-- indexing :: Transducer a (a, Integer)
-- indexing = proc x -> do
--   ???

  

class MonoTransducible t where
  type Elem t :: *
  monoTransduce :: (Elem t ~ a) => Transducer a a -> t -> t

class Transducible t where
  transduce :: Transducer a b -> (b -> r -> r) -> r -> t a -> r

instance Transducible [] where
  transduce (Transducer t) f z x = foldr (t f) z x
  
toList :: Transducer a b -> [a] -> [b]
toList t xs = transduce t (:) [] xs








