module Main where

import MockTasksApi (withServer)
import Protolude
import Spec qualified
import System.IO
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.Runner

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  withTimeout $
    withServer $ \server ->
      hspecWith config (beforeAll (pure server) (parallel Spec.spec))
  where
    config =
      defaultConfig
        { {-
            This may unpack nixpkgs twice, concurrently.
            More than that may be a problem for tmpfs.
            Increase beyond 2 does not cause speedup until Nix has dogpile locking.
          -}
          configConcurrentJobs = Just 2,
          configRandomize = True,
          configColorMode = ColorAlways,
          configUnicodeMode = UnicodeAlways,
          configPrintSlowItems = Just 10,
          configPrintCpuTime = False, -- True would be misleading when the action is in a different process or vm
          configTimes = True -- Also misleading because agent may be saturated arbitrarily, and an arbitrary test will pick up preparatory builds
        }

withTimeout :: IO () -> IO ()
withTimeout =
  let oneSecond = 1000 * 1000
      minute = 60 * oneSecond
   in timeout (15 * minute) >=> \case
        Just _ -> pass
        Nothing -> do
          putText "Test suite timed out!"
          exitFailure
