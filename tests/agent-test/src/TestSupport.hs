module TestSupport where

import Data.List as L
import Data.String
import Protolude
import System.Environment
import System.IO.Unsafe (unsafePerformIO)

apiBaseUrl :: (ConvertText String s, IsString s) => s
apiBaseUrl = unsafePerformIO $ do
  env <- getEnvironment
  let base = maybe "http://api" toS $ L.lookup "BASE_URL" env
  pure base
