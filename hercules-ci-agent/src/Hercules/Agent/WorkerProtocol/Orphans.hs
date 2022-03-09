{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hercules.Agent.WorkerProtocol.Orphans where

import Control.Monad.Fail (fail)
import qualified Data.Aeson as A
import Data.Binary (Binary (get, put))
import Hercules.API.Id (Id (..))
import Hercules.CNix.Expr (ViaJSON (ViaJSON))
import Protolude hiding (get, put)

-- | Orphan
instance Binary (Id (a :: k)) where
  put (Id uuid) = put uuid
  get = Id <$> get

-- | Orphan
instance (A.ToJSON a, A.FromJSON a) => Binary (ViaJSON a) where
  put (ViaJSON a) = put (A.encode a)
  get = do
    bs <- get
    case A.eitherDecode bs of
      Left s -> fail s
      Right r -> pure (ViaJSON r)
