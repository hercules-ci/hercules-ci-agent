{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hercules.API.Labels where

import Control.Lens ((?~))
import Data.Aeson (Value)
import Data.Function ((&))
import Data.OpenApi qualified as O3
import Data.Swagger
import Hercules.API.Prelude

newtype Labels = Labels {fromLabels :: Map Text Value}
  deriving newtype (Show, Eq, NFData, ToJSON, FromJSON)

instance ToSchema Labels where
  declareNamedSchema _p = do
    return
      $ NamedSchema (Just "Labels")
      $ mempty
      & type_ ?~ SwaggerObject
      & additionalProperties ?~ AdditionalPropertiesAllowed True

instance O3.ToSchema Labels where
  declareNamedSchema _p = do
    return
      $ O3.NamedSchema (Just "Labels")
      $ mempty
      & O3.type_ ?~ O3.OpenApiObject
      & O3.additionalProperties ?~ O3.AdditionalPropertiesAllowed True
