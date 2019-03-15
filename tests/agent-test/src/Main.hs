module Main where

import           Protolude
import           Test.Hspec
import           Test.Hspec.Runner
import qualified Spec
import           System.Timeout                 ( timeout )
import           MockTasksApi                   ( withServer )
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  withTimeout $ withServer $ \server ->
    hspecWith config (beforeAll (pure server) Spec.spec)
  where config = defaultConfig { configColorMode = ColorNever }

withTimeout :: IO () -> IO ()
withTimeout =
  let oneSecond = 1000 * 1000
      minute = 60 * oneSecond
  in  timeout (15 * minute) >=> \case
        Just _ -> pass
        Nothing -> do
          putText "Test suite timed out!"
          exitFailure
