{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Hercules.API.Labels where

import Control.Lens ((?~))
import Data.Aeson (Value)
import Data.Function ((&))
import Data.Swagger
import Hercules.API.Prelude

newtype Labels = Labels {fromLabels :: Map Text Value}
  deriving newtype (Show, Eq, NFData, ToJSON, FromJSON)

instance ToSchema Labels where
  declareNamedSchema _p = do
    return $
      NamedSchema (Just "Labels") $
        mempty
          & type_ ?~ SwaggerObject
          & additionalProperties ?~ (AdditionalPropertiesAllowed True)
