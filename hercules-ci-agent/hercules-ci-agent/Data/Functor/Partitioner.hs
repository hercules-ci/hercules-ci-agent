{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Functor.Partitioner
  ( Partitioner (..),
    partitionList,
    part,
    part',

    -- * Working safely with 'Data.Map.Map's
    type WithKey,
    partitionMap,
    partWithKey,
    traversePartWithKey,
  )
where

import Data.Coerce
import qualified Data.Map as M
import Protolude

-- TODO: Profunctor
data Partitioner a b
  = forall m.
    (Monoid m) =>
    Partitioner
      { ingest :: a -> Maybe m,
        digest :: m -> b
      }

partitionList :: Partitioner a b -> [a] -> b
partitionList (Partitioner ing dig) = dig . fold . mapMaybe ing

part :: (a -> Maybe b) -> Partitioner a [b]
part f = Partitioner {ingest = fmap (: []) . f, digest = identity}

-- | Like 'part' but allows returning multiple entries in the result
part' :: (a -> Maybe [b]) -> Partitioner a [b]
part' f = Partitioner {ingest = f, digest = identity}

instance Functor (Partitioner a) where
  fmap f (Partitioner ing dig) = (Partitioner ing (f . dig))

instance Applicative (Partitioner a) where

  pure a =
    Partitioner {ingest = const (Nothing :: Maybe ()), digest = pure a}

  (Partitioner ingp digp) <*> (Partitioner ingq digq) = Partitioner
    { ingest = \a -> case ingp a of
        Just mp -> Just (mp, mempty)
        Nothing -> (mempty,) <$> ingq a,
      digest = \(mp, mq) -> digp mp (digq mq)
    }

-- instance Profunctor Partitioner
-- ...

-- | Encapsulates a 'Data.Map.Map''s key, in order to maintain the invariant all
-- keys and values are preserved when returning @Just@ from 'partWithKey'.
--
-- The problem with Maps is that if you change the keys in a non-injective way,
-- you may accidentally overwrite values. If you do need to change the keys,
-- this module can not guarantee the correctness of your code. You can munge a
-- Map in arbitrary ways, by using 'partitionList' after 'Data.Map.toList', but
-- make sure that your key mapping is injective, or use a newtype wrapper for
-- Map that uses the inner type @a@'s 'Semigroup' instance.
newtype WithKey k v = WithKey (k, v)

partWithKey ::
  Ord k =>
  (k -> v -> Maybe a) ->
  Partitioner (WithKey k v) (Map k a)
partWithKey f = Partitioner
  { ingest = \(WithKey (k, v)) -> M.singleton k <$> f k v,
    digest = identity
  }

traversePartWithKey ::
  (Ord k, Applicative f) =>
  (k -> v -> Maybe (f a)) ->
  Partitioner (WithKey k v) (f (Map k a))
traversePartWithKey f = Partitioner
  { ingest = \(WithKey (k, v)) -> do
      -- Maybe
      x <- f k v
      pure $ Ap $ (M.singleton k <$> x),
    digest = getAp
  }

-- | Use with 'partWithKey' to match on the key.
--
-- It uses 'WithKey' to maintain the invariant that all keys and values are
-- preserved when returning @Just@ from 'partWithKey'.
partitionMap :: Ord k => Partitioner (WithKey k a) b -> M.Map k a -> b
partitionMap p = partitionList p . coerce . M.toAscList
