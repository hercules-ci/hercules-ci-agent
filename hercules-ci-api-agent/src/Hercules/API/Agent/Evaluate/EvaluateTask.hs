{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hercules.API.Agent.Evaluate.EvaluateTask where

import Data.Aeson (Value)
import Hercules.API.Agent.Evaluate.ImmutableInput (ImmutableInput)
import Hercules.API.Prelude
import Hercules.API.Task (Task)

data EvaluateTask = EvaluateTask
  { id :: Id (Task EvaluateTask),
    primaryInput :: Text, -- Obsolete since >= 0.8
    otherInputs :: Map Identifier Text, -- identifier -> HTTP URL
    inputMetadata :: Map Identifier (Map Text Value),
    autoArguments :: Map Text (SubPathOf Identifier), -- argument name -> identifier
    nixPath :: [NixPathElement (SubPathOf Identifier)], -- NIX_PATH element -> identifier
    logToken :: Text,
    selector :: Selector
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

data Selector
  = ConfigOrLegacy
  | OnPush OnPush
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

data OnPush = MkOnPush
  { name :: Text,
    inputs :: Map Text ImmutableInput
  }
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
