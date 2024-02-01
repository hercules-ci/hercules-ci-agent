{-# LANGUAGE BlockArguments #-}

module TestSupport where

import Data.List qualified as L
import Data.Map qualified as M
import Data.String
import Data.UUID.V4 qualified as UUID
import Hercules.API.Id (Id (Id))
import Protolude
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (expectationFailure)

apiBaseUrl :: (ConvertText String s, IsString s) => s
apiBaseUrl = unsafePerformIO $ do
  env <- getEnvironment
  let base = maybe "http://api" toS $ L.lookup "BASE_URL" env
  pure base

randomId :: IO (Id a)
randomId = Id <$> UUID.nextRandom

failWith :: (HasCallStack) => [Char] -> IO a
failWith s = withFrozenCallStack do
  expectationFailure s
  -- Why doesn't it just return forall a? Oh well.
  panic ("expectationFailure didn't throw: " <> toS s)

(=:) :: k -> a -> Map k a
(=:) = M.singleton

printErrItems :: (Foldable t, MonadIO f, Show a) => t a -> f ()
printErrItems items = for_ items \item -> putErrText ("  - " <> show item)

shouldBeJust :: (HasCallStack) => Maybe a -> IO a
shouldBeJust = withFrozenCallStack \case
  Nothing -> do
    failWith "Expected Just, got Nothing"
  Just a -> pure a
