module Hercules.Agent.WorkerProtocol.ViaJSON where

import qualified Data.Aeson as A
import Data.Binary (Binary (..))
import Prelude

newtype ViaJSON a = ViaJSON {fromViaJSON :: a}
  deriving (Eq, Ord, Show, Read)

instance (A.ToJSON a, A.FromJSON a) => Binary (ViaJSON a) where
  put (ViaJSON a) = put (A.encode a)
  get = do
    bs <- get
    case A.eitherDecode bs of
      Left s -> fail s
      Right r -> pure (ViaJSON r)
