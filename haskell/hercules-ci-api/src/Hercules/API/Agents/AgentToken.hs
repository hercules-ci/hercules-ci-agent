{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Agents.AgentToken where

import           Hercules.API.Prelude

import           Hercules.API.Accounts.Account  ( Account )

data AgentToken = AgentToken
  { id :: Id AgentToken
  , ownerId :: Id Account
  , creationTime :: UTCTime
  -- TODO lastAccessTime :: Maybe UTCTime
  , description :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
