{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hercules.Agent.Sensitive where

import Data.Binary
import Protolude
import Text.Show

-- | newtype wrapper to avoid leaking sensitive data through Show
newtype Sensitive a = Sensitive {reveal :: a}
  deriving (Generic)
  deriving newtype (Binary, Eq, Ord, Monoid, Semigroup)
  deriving (Functor, Applicative, Monad) via Identity

-- | @const "<sensitive>"@
instance Show (Sensitive a) where
  show _ = "<sensitive>"

revealContainer :: Functor f => Sensitive (f a) -> f (Sensitive a)
revealContainer (Sensitive fa) = Sensitive <$> fa
