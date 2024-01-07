{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hercules.API.Orphans where

import Control.Lens.Operators ((<>~))
import Data.Data (Typeable)
import Data.Function ((&), (.))
import Data.Maybe (Maybe (Just))
import Data.OpenApi qualified as O3
import Data.Proxy
import Data.Swagger
import Servant.API
import Servant.Auth (Auth, JWT)
import Servant.OpenApi qualified as SO3

-- | Ignores Headers.
--
-- FIXME: don't ignore headers
instance forall a hs. (ToSchema a) => ToSchema (Headers hs a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

-- | Ignores Headers.
--
-- FIXME: don't ignore headers
instance forall a hs. (O3.ToSchema a, Typeable hs) => O3.ToSchema (Headers hs a) where
  declareNamedSchema _ = O3.declareNamedSchema (Proxy @a)

instance (SO3.HasOpenApi a) => SO3.HasOpenApi (Auth '[JWT] x :> a) where
  toOpenApi _ =
    SO3.toOpenApi (Proxy :: Proxy a)
      & O3.security
        <>~ [ O3.SecurityRequirement [("jwt", [])]
            ]
      & O3.components . O3.securitySchemes
        <>~ O3.SecurityDefinitions
          [ ( "jwt",
              O3.SecurityScheme
                (O3.SecuritySchemeHttp (O3.HttpSchemeBearer (Just "jwt")))
                (Just "JSON Web Token authentication")
            )
          ]
