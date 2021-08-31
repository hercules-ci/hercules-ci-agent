{-# LANGUAGE BlockArguments #-}

import Hercules.CNix.Expr (init)
import Protolude
import SingleState
import qualified Spec
import System.Mem (performMajorGC)
import Test.Hspec.Runner

main :: IO ()
main = do
  init
  withGlobalState $ do
    -- for_ [(1 :: Int)..1000] \_ -> do
    hspecWith config Spec.spec
    --  `catch` (\e -> putErrText $ "Caught  " <> show (e :: SomeException))
    putErrText "Performing Haskell GC..."
    performMajorGC
    putErrText "Haskell GC done..."
  where
    config =
      defaultConfig
        { configColorMode = ColorAlways
        }
