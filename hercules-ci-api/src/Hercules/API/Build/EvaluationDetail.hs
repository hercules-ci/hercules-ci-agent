{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Build.EvaluationDetail where

import Hercules.API.Attribute (Attribute)
import Hercules.API.Build.EvaluationDependency
  ( EvaluationDependency,
  )
import Hercules.API.Derivation (Derivation, DerivationStatus)
import Hercules.API.Evaluation.AttributeError
  ( AttributeError,
  )
import Hercules.API.Evaluation.Evaluation
  ( Evaluation,
  )
import Hercules.API.Message (Message)
import Hercules.API.Prelude
import Hercules.API.Result (Result)

data EvaluationDetail
  = EvaluationDetail
      { id :: Id Evaluation,
        messages :: [Message],
        attributes :: [Attribute (Result AttributeError Derivation)],
        evaluationDependencies :: [EvaluationDependency],
        -- | A set of (path, derivationstatus) that is relevant to the evaluation
        derivations :: Map Text DerivationStatus,
        derivationsWaitingCount :: Int,
        derivationsBuildingCount :: Int,
        derivationsBuildFailureCount :: Int,
        derivationsDependencyFailureCount :: Int,
        derivationsBuildSuccessCount :: Int,
        derivationsCancelledCount :: Int
      }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
