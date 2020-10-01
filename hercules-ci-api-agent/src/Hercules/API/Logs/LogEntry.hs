{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Hercules.API.Logs.LogEntry where

import Control.Applicative
import qualified Data.Aeson.Types as A
import Data.Vector (Vector)
import Data.Word (Word64)
import Hercules.API.Prelude

newtype ActivityId = ActivityId Word64
  deriving newtype (ToJSON, FromJSON, Show, Eq, NFData)

newtype ActivityType = ActivityType Word64
  deriving newtype (ToJSON, FromJSON, Show, Eq, NFData)

newtype ResultType = ResultType Word64
  deriving newtype (ToJSON, FromJSON, Show, Eq, NFData)

pattern ResultTypeProgress :: ResultType
pattern ResultTypeProgress = ResultType 105

data Field = Int !Word64 | String !Text
  deriving (Eq, Show, Generic, NFData)

instance ToJSON Field where
  toJSON (Int int) = A.toJSON int
  toJSON (String s) = A.toJSON s

instance FromJSON Field where
  parseJSON v =
    Int <$> A.parseJSON v
      <|> String <$> A.parseJSON v

data LogEntry
  = Msg
      { i :: !Word64,
        ms :: !Word64,
        level :: !Int,
        msg :: !Text
      }
  | Start
      { i :: !Word64,
        ms :: !Word64,
        act :: !ActivityId,
        level :: !Int,
        typ :: !ActivityType,
        text :: !Text,
        fields :: !(Vector Field),
        parent :: !ActivityId
      }
  | Stop
      { i :: !Word64,
        ms :: !Word64,
        act :: !ActivityId
      }
  | Result
      { i :: !Word64,
        ms :: !Word64,
        act :: !ActivityId,
        rtype :: !ResultType,
        fields :: !(Vector Field)
      }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
