module Main where

import CNix
import Protolude
import qualified Spec
import Test.Hspec.Runner

main :: IO ()
main = do
  CNix.init
  hspecWith config Spec.spec
  where
    config = defaultConfig {configColorMode = ColorAlways}
