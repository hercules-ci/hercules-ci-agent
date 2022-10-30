{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hercules.Agent.WorkerProtocol.Event where

import Control.Monad (fail)
import qualified Data.Aeson as A
import Data.Binary
import Data.UUID (UUID)
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnPushHandlerEvent (OnPushHandlerEvent)
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnScheduleHandlerEvent (OnScheduleHandlerEvent)
import Hercules.Agent.WorkerProtocol.Event.Attribute
import Hercules.Agent.WorkerProtocol.Event.AttributeError
import Hercules.Agent.WorkerProtocol.Event.AttributeIFD (AttributeIFD)
import Hercules.Agent.WorkerProtocol.Event.BuildResult
import Protolude hiding (get, put)
import Prelude ()

data Event
  = Attribute Attribute
  | AttributeError AttributeError
  | AttributeIFD AttributeIFD
  | EvaluationDone
  | Error Text
  | Build ByteString Text (Maybe UUID) Bool
  | BuildResult BuildResult
  | EffectResult Int
  | JobConfig
  | OnPushHandler (ViaJSON OnPushHandlerEvent)
  | OnScheduleHandler (ViaJSON OnScheduleHandlerEvent)
  | Exception Text
  deriving (Generic, Binary, Show, Eq)

newtype ViaJSON a = ViaJSON {fromViaJSON :: a}
  deriving (Eq, Ord, Show, Read)

-- | Orphan
instance (A.ToJSON a, A.FromJSON a) => Binary (ViaJSON a) where
  put (ViaJSON a) = put (A.encode a)
  get = do
    bs <- get
    case A.eitherDecode bs of
      Left s -> fail s
      Right r -> pure (ViaJSON r)
