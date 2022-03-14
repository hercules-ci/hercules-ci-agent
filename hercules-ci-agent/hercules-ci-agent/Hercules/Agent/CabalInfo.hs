module Hercules.Agent.CabalInfo (herculesAgentVersion) where

import Data.Version (showVersion)
import Paths_hercules_ci_agent (version)
import Protolude

herculesAgentVersion :: Text
herculesAgentVersion = toS (showVersion version)
