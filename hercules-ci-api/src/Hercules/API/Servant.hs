{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Hercules.API.Servant where

import Data.Text (Text)
import Servant.API

type Get302 (cts :: [*]) (hs :: [*]) = Verb 'GET 302 cts (Headers (Header "Location" Text ': hs) NoContent)

type Post302 (cts :: [*]) (hs :: [*]) = Verb 'POST 302 cts (Headers (Header "Location" Text ': hs) NoContent)
