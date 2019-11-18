module Main where

import qualified Spec
import Test.Hspec.Runner
import Prelude

main :: IO ()
main = hspecWith config Spec.spec
  where
    config = defaultConfig {configColorMode = ColorAlways}
