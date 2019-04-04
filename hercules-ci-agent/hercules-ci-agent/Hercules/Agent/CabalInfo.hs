module Hercules.Agent.CabalInfo where

import           Data.Version                   ( showVersion )
import           Protolude
import           Paths_hercules_ci_agent        ( version )

herculesAgentVersion :: Text
herculesAgentVersion = toS (showVersion version)
