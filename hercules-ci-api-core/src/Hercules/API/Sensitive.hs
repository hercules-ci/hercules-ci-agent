{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hercules.API.Sensitive where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity (Identity))
import GHC.Generics (Generic)
import Prelude

-- | newtype wrapper to avoid leaking sensitive data through 'Show'
newtype Sensitive a = Sensitive {reveal :: a}
  deriving (Generic)
  deriving newtype (Eq, Ord, Monoid, Semigroup, ToJSON, FromJSON)
  deriving (Functor, Applicative, Monad) via Identity

-- | @const "<sensitive>"@
instance Show (Sensitive a) where
  show _ = "<sensitive>"

revealContainer :: (Functor f) => Sensitive (f a) -> f (Sensitive a)
revealContainer (Sensitive fa) = Sensitive <$> fa
