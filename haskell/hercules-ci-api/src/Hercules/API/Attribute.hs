{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hercules.API.Attribute where

import           Prelude
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.Swagger                   ( ToSchema )

data Attribute a = Attribute
  { path :: [Text]
  , value :: a
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
deriving instance ToSchema a => ToSchema (Attribute a)
deriving instance Functor Attribute
deriving instance Foldable Attribute
deriving instance Traversable Attribute
