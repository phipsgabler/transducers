{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Data.Transducers (
    Transducer (..)
  , mapping
  , filtering
  , concatenating
  , flatmapping
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
                                     \case Left b -> \r -> let cons' = t $ \c r -> cons (Left c) r
                                                           in cons' b r
                                           Right d -> \r -> cons (Right d) r)

-- unnecessary in theory (cf. WrappedArrow), but instructional.
instance Profunctor Transducer where
  lmap f (Transducer t) = Transducer (\cons -> \a r -> (t cons) (f a) r)
  rmap f (Transducer t) = Transducer (\cons -> \a r -> (t $ \b -> cons (f b)) a r )




mapping :: (a -> b) -> Transducer a b
mapping f = Transducer (\cons -> \a r -> cons (f a) r)

filtering :: (a -> Bool) -> Transducer a a
filtering p = Transducer (\cons -> \a r -> if p a then cons a r else r)

concatenating :: Foldable t => Transducer (t a) a
concatenating = Transducer (\cons -> \as r -> foldr cons r as)

flatmapping :: Foldable t => (a -> t b) -> Transducer a b
flatmapping f = mapping f >>> concatenating




class MonoTransducible t where
  type Elem t :: *
  monoTransduce :: (Elem t ~ a) => Transducer a a -> t -> t

class Transducible t where
  transduce :: Transducer a b -> (b -> r -> r) -> r -> t a -> r

instance Transducible [] where
  transduce (Transducer t) f z x = foldr (t f) z x
  
toList :: Transducer a b -> [a] -> [b]
toList t xs = transduce t (:) [] xs








