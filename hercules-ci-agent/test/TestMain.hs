module Main where

import Hercules.CNix (init)
import Protolude
import Spec qualified
import Test.Hspec.Runner

main :: IO ()
main = do
  init
  hspecWith config Spec.spec
  where
    config = defaultConfig {configColorMode = ColorAlways}
