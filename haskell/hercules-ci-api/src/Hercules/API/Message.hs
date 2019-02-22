{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Message where

import           Prelude
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Swagger                   ( ToSchema )

data Message = Message
  { index :: Int
  , typ :: Type
  , message :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data Type
  = Error -- ^ Something went wrong, inform user about possible
          -- cause. Examples: source could not be fetched, could not
          -- find a nix expression file to call.

  | Trace -- ^ The nix expression contained a @builtins.trace@
          -- call. Ideally we should keep track of during which
          -- attribute it was encountered. It is not an attribute
          -- property because we can not reasonably know which
          -- attributes (plural) trigger the evaluation of
          -- @trace@. Indeed side effecting evaluation breaks the
          -- abstraction.
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
