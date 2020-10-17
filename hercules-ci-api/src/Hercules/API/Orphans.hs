{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hercules.API.Orphans where

import Data.Proxy
import Data.Swagger
import Servant.API

-- | Ignores Headers.
--
-- FIXME: don't ignore headers
instance forall a hs. ToSchema a => ToSchema (Headers hs a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)
