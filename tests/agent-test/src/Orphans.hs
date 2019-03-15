{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Orphans where

#if ! MIN_VERSION_http_api_data(0,3,8)

import           Protolude

import           Web.HttpApiData
import           Web.Cookie                   (SetCookie,
                                               renderSetCookie)
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as LBS
import           Data.Text.Encoding           (decodeUtf8With)
import           Data.Text.Encoding.Error     (lenientDecode)

instance ToHttpApiData SetCookie where
  toUrlPiece = decodeUtf8With lenientDecode . toHeader
  toHeader = LBS.toStrict . BB.toLazyByteString . renderSetCookie

#endif
