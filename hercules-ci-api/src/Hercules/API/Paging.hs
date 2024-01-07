{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hercules.API.Paging where

import Data.OpenApi qualified as O3
import Hercules.API.Prelude

-- To be used in newtypes only; otherwise the schema will have colliding
-- PagedResponse types.
data PagedResponse a = PagedResponse
  { -- | The items you requested, up to some limit.
    items :: [a],
    -- | Whether more items would be returned if it wasn't for the limit.
    more :: Bool
  }
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

deriving instance (ToSchema a) => ToSchema (PagedResponse a)

deriving instance (O3.ToSchema a) => O3.ToSchema (PagedResponse a)
