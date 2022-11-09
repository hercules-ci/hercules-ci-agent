{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEvent where

import Hercules.API.Prelude

data AttributeEffectEvent = AttributeEffectEvent
  { expressionPath :: [Text],
    derivationPath :: Text,
    secretsToUse :: Map Text SecretRef
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

-- | The right hand side of the @secretsToUse@; how to get the secret.
data SecretRef
  = -- | Retrieve a secret from @secrets.json@.
    SimpleSecret SimpleSecret
  | -- | Retrieve a token for the current repository.
    GitToken GitToken
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

data SimpleSecret = MkSimpleSecret {name :: Text}
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

data GitToken = MkGitToken {}
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)
