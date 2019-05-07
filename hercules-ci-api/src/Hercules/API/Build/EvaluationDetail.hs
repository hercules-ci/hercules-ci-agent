{-# LANGUAGE DeriveAnyClass #-}
module Hercules.API.Build.EvaluationDetail where

import           Hercules.API.Prelude
import           Hercules.API.Result            ( Result )
import           Hercules.API.Attribute         ( Attribute )
import           Hercules.API.Derivation        ( Derivation )
import           Hercules.API.Evaluation.Evaluation
                                                ( Evaluation )
import           Hercules.API.Message           ( Message )

data EvaluationDetail = EvaluationDetail
  { id :: Id Evaluation
  , messages :: [Message]
  , attributes :: [Attribute (Result Text Derivation)]
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
