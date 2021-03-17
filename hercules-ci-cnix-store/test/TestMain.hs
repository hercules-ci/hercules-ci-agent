{-# LANGUAGE BlockArguments #-}

import Hercules.CNix (init)
import Protolude
import qualified Spec
import System.Mem (performMajorGC)
import Test.Hspec.Runner

main :: IO ()
main = do
  init
  hspecWith config Spec.spec
  performMajorGC
  where
    config =
      defaultConfig
        { configColorMode = ColorAlways
        }
