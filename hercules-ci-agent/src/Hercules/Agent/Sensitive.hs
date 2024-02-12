{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hercules.Agent.Sensitive (module Hercules.API.Sensitive) where

import Data.Binary
import Hercules.API.Sensitive

deriving newtype instance (Binary a) => Binary (Sensitive a)
