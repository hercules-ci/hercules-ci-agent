{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
module Hercules.API.Result
  ( Result(..)
  , toEither
  , fromEither
  , either
  )
where

import           Prelude                 hiding ( either )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Swagger                   ( ToSchema )
import           Data.Profunctor                ( Profunctor
                                                , dimap
                                                )

data Result e a
  = Ok a
  | Error e
  deriving (Generic, Show, Read, Eq, Ord, Functor, Foldable, Traversable, ToJSON, FromJSON)
deriving instance (ToSchema e, ToSchema a) => ToSchema (Result e a)
-- many more typeclasses can be implemented

either :: Iso (Result e a) (Result e' a') (Either e a) (Either e' a')
either = iso toEither fromEither

toEither :: Result e a -> Either e a
toEither (Ok a) = Right a
toEither (Error e) = Left e

fromEither :: Either e a -> Result e a
fromEither (Right a) = Ok a
fromEither (Left e) = Error e

-- | See @lens@ package.
type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
