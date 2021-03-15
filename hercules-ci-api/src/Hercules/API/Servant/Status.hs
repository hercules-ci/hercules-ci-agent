{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Hercules.API.Servant.Status where

import Data.Kind (Type)
import Hercules.API.Prelude
import Servant.API

type Get302 (cts :: [Type]) (hs :: [Type]) = Verb 'GET 302 cts (Headers (Header "Location" Text ': hs) NoContent)

type Post302 (cts :: [Type]) (hs :: [Type]) = Verb 'POST 302 cts (Headers (Header "Location" Text ': hs) NoContent)
