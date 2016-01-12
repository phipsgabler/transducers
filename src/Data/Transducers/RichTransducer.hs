{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module Data.Transducers.RichTransducer where


import Prelude hiding ((.), id, Functor(..))
import Control.Category
import Control.Arrow
import Data.Profunctor



-- original (https://github.com/clojure/clojure/blob/bdc752a7fefff5e63e0847836ae5e6d95f971c37/src/clj/clojure/core.clj#L2596)
-- (defn map
--   ([f]
--     (fn [rf]
--       (fn
--         ([] (rf))
--         ([result] (rf result))
--         ([result input]
--            (rf result (f input)))
--         ([result input & inputs]
--            (rf result (apply f input inputs)))))))

-- like in https://www.reddit.com/r/haskell/comments/2cv6l4/clojures_transducers_are_perverse_lenses/cjjnuch
data Reducer :: * -> * -> * where
  Reducer :: (a -> r -> r) -> r -> (r -> x) -> Reducer a r

  
newtype RichTransducer a b = RichTransducer (forall r. Reducer b r -> Reducer a r)

instance Category RichTransducer where
  id = RichTransducer (\red -> red)
  (RichTransducer t1) . (RichTransducer t2) =  RichTransducer (\red -> t2 . t1 $ red)

instance Arrow RichTransducer where
  arr f = RichTransducer $ \(Reducer cons nil fin) -> Reducer (\a r -> cons (f a) r) nil fin
  first (RichTransducer t) = RichTransducer $ \(Reducer cons nil fin) ->
    let cons' = \(b, d) r -> case t $ Reducer (\c r -> cons (c, d) r) nil fin of
          Reducer cons'' nil'' fin'' -> cons'' b r
    in Reducer cons' nil fin
  -- Transducer (\cons -> \(b,d) r -> let cons' = t $ \c r -> cons (c, d) r
  --                                  in cons' b r)

-- instance ArrowChoice RichTransducer where
--   left (RichTransducer t) = RichTransducer (\cons ->
--                                      \case Left b -> \r -> (t $ cons . Left) b r
--                                            Right d -> \r -> cons (Right d) r)

-- unnecessary in theory (cf. WrappedArrow), but instructional.
instance Profunctor RichTransducer where
  lmap f (RichTransducer t) = RichTransducer $ \r -> case t r of
    Reducer cons nil fin -> Reducer (\c r -> cons (f c) r) nil fin
  rmap f (RichTransducer t) = RichTransducer $
    \(Reducer cons nil fin) -> t $ Reducer (cons . f) nil fin



-- instance ArrowLoop (->) where
--     loop :: ((b,d) -> (c,d)) -> (b -> c)
--     loop f b = let (c,d) = f (b,d) in c
-- looping :: Transducer (a, l) (b, l) -> Transducer a b
-- looping (Transducer t) = Transducer (\cons -> \a r ->
--                                       let f = (flip $ t $ \(b, l) r -> cons b r) r
--                                           --(x, y) = f (
--                                       in _)
