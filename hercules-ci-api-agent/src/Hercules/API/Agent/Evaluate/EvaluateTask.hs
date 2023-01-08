{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hercules.API.Agent.Evaluate.EvaluateTask where

import Data.Aeson (Value)
import qualified Hercules.API.Agent.Evaluate.EvaluateTask.OnPush as OnPush
import qualified Hercules.API.Agent.Evaluate.EvaluateTask.OnSchedule as OnSchedule
import Hercules.API.Agent.Evaluate.ImmutableInput (ImmutableInput)
import Hercules.API.Prelude
import Hercules.API.Task (Task)

data EvaluateTask = EvaluateTask
  { id :: Id (Task EvaluateTask),
    primaryInput :: Text, -- Obsolete since >= 0.8
    otherInputs :: Map Identifier Text, -- identifier -> HTTP URL
    inputMetadata :: Map Identifier (Map Text Value),
    inputs :: Map Identifier ImmutableInput,
    autoArguments :: Map Text (SubPathOf Identifier), -- argument name -> identifier
    nixPath :: [NixPathElement (SubPathOf Identifier)], -- NIX_PATH element -> identifier
    logToken :: Text,
    selector :: Selector,
    ciSystems :: Maybe (Map Text ()),
    extraGitCredentials :: Maybe [Credential],
    -- | Whether to use Nix's fetching mechanism for everything.
    --
    -- Putting checkouts in the store isn't always desirable, so we keep the
    -- non-flake behavior of custom checkouts for non-flake use cases.
    isFlakeJob :: Bool
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

data Credential = Credential
  { url :: Text,
    username :: Text,
    password :: Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

data Selector
  = ConfigOrLegacy
  | OnPush OnPush.OnPush
  | OnSchedule OnSchedule.OnSchedule
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

type Identifier = Text

data NixPathElement a = NixPathElement
  { -- | for example @/home/user/nixpkgs@ in @/home/user/nixpkgs:/etc/nixos/foo@
    prefix :: Maybe Text,
    value :: a
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, Functor, Foldable, Traversable)

-- | For using a path inside a source
data SubPathOf a = SubPathOf
  { path :: a,
    subPath :: Maybe Text
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON, Functor, Foldable, Traversable)
