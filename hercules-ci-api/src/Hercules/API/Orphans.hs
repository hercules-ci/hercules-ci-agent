{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Hercules.API.Orphans where

import           Servant.Auth.Swagger           ( )

#if !MIN_VERSION_swagger2(2,3,1)
import           Data.Monoid                    ( mempty )
import           Data.Swagger
import           Web.Cookie                     ( SetCookie )

-- nixpkgs <= 18.09
instance ToParamSchema SetCookie where
  toParamSchema _ = mempty -- TODO: cookie instances for swagger
#endif
