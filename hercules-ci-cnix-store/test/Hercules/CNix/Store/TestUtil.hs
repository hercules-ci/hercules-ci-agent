{-# LANGUAGE BlockArguments #-}

module Hercules.CNix.Store.TestUtil where

import Hercules.CNix.Store (Store, withStoreFromURI)
import Protolude
import System.IO.Temp (withSystemTempDirectory)

withTempStore :: (Store -> IO a) -> IO a
withTempStore f =
  withSystemTempDirectory "cnix-test-store" \d ->
    withStoreFromURI (toS d) f
