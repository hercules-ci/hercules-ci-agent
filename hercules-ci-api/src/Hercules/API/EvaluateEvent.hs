{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.EvaluateEvent where

import           Prelude
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Swagger                   ( ToSchema )
import qualified Hercules.API.EvaluateEvent.AttributeEvent
                                               as AttributeEvent
import qualified Hercules.API.EvaluateEvent.AttributeErrorEvent
                                               as AttributeErrorEvent
import qualified Hercules.API.Message          as Message

data EvaluateEvent
  = Attribute AttributeEvent.AttributeEvent
  | AttributeError AttributeErrorEvent.AttributeErrorEvent
  | Message Message.Message
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
