{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Formats.Mountable where

import qualified Data.Aeson as A
import Data.Text (Text)
import GHC.Generics (Generic)
import Hercules.Formats.Secret (Condition)
import Prelude (Bool, Eq, Show)

data Mountable = Mountable
  { -- | A path on the host.
    source :: !Text,
    readOnly :: !Bool,
    condition :: !Condition
  }
  deriving (Generic, Show, Eq, A.ToJSON, A.FromJSON)
