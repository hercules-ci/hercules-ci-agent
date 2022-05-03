{-# LANGUAGE BlockArguments #-}

import Hercules.CNix.Expr (init)
import Protolude
import SingleState
import qualified Spec
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performMajorGC)
import Test.Hspec.Runner

main :: IO ()
main = withTempHome do
  init
  withGlobalState $ do
    -- for_ [(1 :: Int)..1000] \_ -> do
    hspecWith config {configConcurrentJobs = Just 1} Spec.spec
    --  `catch` (\e -> putErrText $ "Caught  " <> show (e :: SomeException))
    putErrText "Performing Haskell GC..."
    performMajorGC
    putErrText "Haskell GC done..."
  where
    config =
      defaultConfig
        { configColorMode = ColorAlways
        }

-- Perfect for any housing crisis
withTempHome :: IO () -> IO ()
withTempHome io =
  withSystemTempDirectory "test-home" \home -> do
    -- HOME may not affect Nix getHome(), as its static variable may already be bound
    setEnv "HOME" home

    setEnv "XDG_CACHE_HOME" (home </> ".cache")
    setEnv "XDG_CONFIG_HOME" (home </> ".config")
    setEnv "XDG_DATA_HOME" (home </> ".local" </> "share")

    io
