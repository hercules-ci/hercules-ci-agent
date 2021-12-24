module Hercules.Agent.Binary where

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Extra as BL
import System.IO (Handle)
import Prelude

-- As recommended in the binary docs, taken from https://hackage.haskell.org/package/binary-0.8.9.0/docs/src/Data.Binary.html#decodeFileOrFail

-- | Decode a value from a 'Handle'. Returning 'Left' on failure and 'Right' on success.
-- In case of failure, the unconsumed input and a human-readable error message will be returned.
decodeBinaryFromHandle :: Binary a => Handle -> IO (Either (BS.ByteString, ByteOffset, String) a)
decodeBinaryFromHandle = feed (runGetIncremental get)
  where
    feed (Done _ _ x) _ = pure (Right x)
    feed (Fail unconsumed pos str) _ = pure (Left (unconsumed, pos, str))
    feed (Partial k) h = do
      chunk <- BS.hGetSome h BL.defaultChunkSize
      case BS.length chunk of
        0 -> feed (k Nothing) h
        _ -> feed (k (Just chunk)) h
