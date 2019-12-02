{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hercules.Agent.Bag
  ( ParseBag (..),
    parseBag,
    part,
    whenKind,
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Functor.Compose
import Data.Functor.Partitioner hiding
  ( part,
  )
import qualified Data.HashMap.Strict as HashMap
import Protolude

-- TODO: Use a Validation instead of Either to return all errors at once

-- | Partitioning and validation for heterogeneous JSON maps
newtype ParseBag a b = ParseBag {getReadBag :: Compose (Partitioner (WithKey Text a)) Parser b}
  deriving (Functor, Applicative)

-- | Text argument: Map key, a: thing you're parsing. Return 'Nothing' to skip the object and let another part handle it.
part :: (Text -> a -> Maybe (Parser b)) -> ParseBag a (Map Text b)
part f = ParseBag $ Compose $ traversePartWithKey f

parseBag :: ParseBag Value a -> Value -> Parser a
parseBag f v = do
  m <- parseJSON v
  partitionMap (getCompose $ getReadBag f) m

-- | Ignore if the value is not an object or if it doesn't have a "kind" field
-- set to the provided kind.
whenKind :: Text -> (Value -> Maybe a) -> Value -> Maybe a
whenKind expectedKind f v@(Object o) =
  ( do
      x <- HashMap.lookup "kind" o
      guard (x == String expectedKind)
  )
    *> f v
whenKind _ _ _ = Nothing
