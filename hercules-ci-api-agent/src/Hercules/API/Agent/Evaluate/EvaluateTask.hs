{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hercules.API.Agent.Evaluate.EvaluateTask where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value,
  )
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hercules.API.Id
import Hercules.API.Task (Task)
import Prelude

data EvaluateTask
  = EvaluateTask
      { id :: Id (Task EvaluateTask),
        primaryInput :: Text, -- Obsolete since >= 0.8
        otherInputs :: Map Identifier Text, -- identifier -> HTTP URL
        inputMetadata :: Map Identifier (Map Text Value),
        autoArguments :: Map Text (SubPathOf Identifier), -- argument name -> identifier
        nixPath :: [NixPathElement (SubPathOf Identifier)], -- NIX_PATH element -> identifier
        logToken :: Text
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

type Identifier = Text

data NixPathElement a
  = NixPathElement
      { -- | for example @/home/user/nixpkgs@ in @/home/user/nixpkgs:/etc/nixos/foo@
        prefix :: Maybe Text,
        value :: a
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, Functor, Foldable, Traversable)

-- | For using a path inside a source
data SubPathOf a
  = SubPathOf
      { path :: a,
        subPath :: Maybe Text
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, Functor, Foldable, Traversable)
