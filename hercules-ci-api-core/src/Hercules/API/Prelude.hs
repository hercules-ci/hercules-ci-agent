-- | Re-exports for the basic types that are used throughout the API
module Hercules.API.Prelude
  ( -- * Re-exports
    module Prelude,

    -- * Types
    Id,
    Name,
    Int64,
    Map,
    Set,
    Text,
    UTCTime,

    -- * Type classes
    Generic,
    Typeable,
    NFData (..),
    ToJSON,
    FromJSON,
    ToSchema,
    schemaCompatibleOptions,

    -- * Servant
    Hercules.API.Servant.Substitute,
    Hercules.API.Servant.Placeholder,
    Hercules.API.Servant.enterApiE,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import qualified Data.Aeson as Aeson
import Data.Int (Int64)
import Data.Map (Map)
import Data.Set (Set)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Hercules.API.Id (Id)
import Hercules.API.Name (Name)
import qualified Hercules.API.Servant
import Prelude

schemaCompatibleOptions :: Aeson.Options
schemaCompatibleOptions =
  Aeson.defaultOptions
    { Aeson.sumEncoding = Aeson.ObjectWithSingleField
    }
