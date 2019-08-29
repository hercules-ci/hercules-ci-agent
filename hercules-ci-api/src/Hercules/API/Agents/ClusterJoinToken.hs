{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agents.ClusterJoinToken where

import Hercules.API.Accounts.Account (Account)
import Hercules.API.Prelude

data ClusterJoinToken
  = ClusterJoinToken
      { id :: Id ClusterJoinToken,
        ownerId :: Id Account,
        creationTime :: UTCTime,
        -- TODO lastAccessTime :: Maybe UTCTime
        description :: Text
        }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
