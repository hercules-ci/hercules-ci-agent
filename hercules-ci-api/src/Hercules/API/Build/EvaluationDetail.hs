{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Build.EvaluationDetail where

import Hercules.API.Attribute (Attribute)
import Hercules.API.Build.EvaluationDependency
  ( EvaluationDependency
    )
import Hercules.API.Derivation (Derivation)
import Hercules.API.Evaluation.AttributeError
  ( AttributeError
    )
import Hercules.API.Evaluation.Evaluation
  ( Evaluation
    )
import Hercules.API.Message (Message)
import Hercules.API.Prelude
import Hercules.API.Result (Result)

data EvaluationDetail
  = EvaluationDetail
      { id :: Id Evaluation,
        messages :: [Message],
        attributes :: [Attribute (Result AttributeError Derivation)],
        evaluationDependencies :: [EvaluationDependency]
        }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
