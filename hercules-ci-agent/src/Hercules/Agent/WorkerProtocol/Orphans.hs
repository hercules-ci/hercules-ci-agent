{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hercules.Agent.WorkerProtocol.Orphans where

import Control.Applicative ((<$>))
import Data.Binary
import Hercules.API.Id (Id (..))

instance Binary (Id (a :: k)) where
  put (Id uuid) = put uuid
  get = Id <$> get
