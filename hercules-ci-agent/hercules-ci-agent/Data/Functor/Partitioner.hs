{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Functor.Partitioner where

import           Protolude

data Partitioner a b =
   forall m. (Monoid m) =>
   Partitioner
   { ingest :: a -> Maybe m
   , digest :: m -> b
   }

partitionList :: Partitioner a b -> [a] -> b
partitionList (Partitioner ing dig) = dig . fold . mapMaybe ing

part :: (a -> Maybe b) -> Partitioner a [b]
part f = Partitioner { ingest = fmap (: []) . f, digest = identity }

part' :: (a -> Maybe [b]) -> Partitioner a [b]
part' f = Partitioner { ingest = f, digest = identity }

instance Functor (Partitioner a) where
  fmap f (Partitioner ing dig) = (Partitioner ing (f . dig))
instance Applicative (Partitioner a) where
  pure a =
    Partitioner { ingest = const (Nothing :: Maybe ()), digest = pure a }
  (Partitioner ingp digp) <*> (Partitioner ingq digq) = Partitioner
    { ingest = \a -> case ingp a of
                 Just mp -> Just (mp, mempty)
                 Nothing -> (mempty, ) <$> ingq a
    , digest = \(mp, mq) -> digp mp (digq mq)
    }
-- instance Profunctor Partitioner
-- ...
