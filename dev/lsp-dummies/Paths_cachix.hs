module Paths_cachix where

import Data.Version
import Prelude

version :: Version
version = Version [0] []

getBinDir :: IO FilePath
getBinDir = pure "/bad/lsp/bindir"
