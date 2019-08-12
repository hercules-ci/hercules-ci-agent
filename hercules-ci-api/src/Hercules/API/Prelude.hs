-- | Re-exports for the basic types that are used throughout the API
module Hercules.API.Prelude
  ( -- * Re-exports
    module Prelude
    -- * Types
  , Id
  , Name
  , Int64
  , Map
  , Set
  , Text
  , UTCTime
    -- * Type classes
  , Generic
  , ToJSON
  , FromJSON
  , ToSchema
  , schemaCompatibleOptions
  )
where

import           Prelude

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Int                       ( Int64 )
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Data.Swagger                   ( ToSchema )
import           GHC.Generics                   ( Generic )
import           Hercules.API.Id                ( Id )
import           Hercules.API.Name              ( Name )

schemaCompatibleOptions :: Aeson.Options
schemaCompatibleOptions = Aeson.defaultOptions
  { Aeson.sumEncoding = Aeson.ObjectWithSingleField
  }
