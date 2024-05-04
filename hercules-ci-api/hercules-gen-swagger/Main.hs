{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Lens (over, (?~))
import Data.Aeson (encode)
import Data.Function ((&))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap, mapWithKey)
import Data.Maybe (fromMaybe)
import Data.OpenApi (HasPaths (paths), OpenApi, Operation, PathItem, operationId)
import Data.OpenApi qualified as O
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Text qualified as T
import Hercules.API (openapi3, swagger)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--experimental-openapi3"] ->
      putStrLn $
        openapi3
          & transferOperationIds
          & encode
          & toS
          & T.replace "Attribute_(Result_AttributeError_Derivation)" "Attribute_Result_AttributeError_Derivation"
          & toS
    [] -> putStrLn $ toS $ encode swagger
    _ -> do
      hPutStrLn stderr "Usage: hercules-gen-swagger [--experimental-openapi3] > swagger.json"
      error $ "Unknown arguments" <> show args

transferOperationIds :: OpenApi -> OpenApi
transferOperationIds = over (paths) fPathItems
  where
    fPathItems :: InsOrdHashMap FilePath PathItem -> InsOrdHashMap FilePath PathItem
    fPathItems = mapWithKey fPathItem

    fPathItem :: FilePath -> PathItem -> PathItem
    fPathItem p pit =
      pit
        & over (O.get . traverse) (fOperation p "get")
          . over (O.put . traverse) (fOperation p "put")
          . over (O.post . traverse) (fOperation p "post")
          . over (O.delete . traverse) (fOperation p "delete")
          . over (O.options . traverse) (fOperation p "options")
          . over (O.head_ . traverse) (fOperation p "head")
          . over (O.patch . traverse) (fOperation p "patch")
          . over (O.trace . traverse) (fOperation p "trace")

    fOperation :: FilePath -> Text -> Operation -> Operation
    fOperation p method o = o & operationId ?~ makeId p method o

    makeId :: FilePath -> Text -> Operation -> Text
    makeId path method _o = pathPart <> "_" <> method
      where
        pathPart =
          toS path
            & T.stripPrefix "/api/v1/"
            & fromMaybe (toS path)
            -- Resources by id
            & T.replace "accounts/{accountId}" "account_by_id"
            & T.replace "organizations/{organizationId}" "organization_by_id"
            & T.replace "projects/{projectId}" "project_by_id"
            & T.replace "jobs/{jobId}" "job_by_id"
            & T.replace "cli_tokens/{cliTokenId}" "cli_token_by_id"
            & T.replace "forges/{forgeId}" "forge_by_id"
            & T.replace "sites/{forgeId}" "site_forge_by_id" -- legacy
            & T.replace "gitlab/installation/{installationId}" "gitlab_installation_by_id"
            & T.replace "clusterJoinTokens/{clusterJoinTokenId}" "cluster_join_token_by_id"
            & T.replace "lock-leases/{lockLeaseId}" "lock_lease_by_id"
            & T.replace "installation/{installationId}" "installation_by_id"
            & T.replace "auth/cli/tokens/{cliTokenId}" "auth_cli_token_by_id"
            -- Resources by name
            & T.replace "forge/{forgeName}" "forge_by_name"
            & T.replace "state/{stateName}" "state_by_name"
            & T.replace "lock/{lockName}" "lock_by_name"
            -- Resources by multiple names
            & T.replace "site/{site}/account/{account}/project/{project}" "project_by_names"
            & T.replace "site/{site}/account/{account}" "account_by_names"
            -- Resources by other key
            & T.replace "derivations/{derivationPath}" "derivation_by_path"
            & T.replace "authorization/request/{browserToken}" "authorization_request_by_browser_token"
            & T.replace "auth/cli/authorization/request/status/{temporaryToken}" "auth_cli_authorization_request_status_by_temporary_token"
            & T.replace "on-schedule/{jobName}" "on_schedule_by_job_name"
            & T.replace "evaluation/compare/{baseJobId}" "evaluation_compare_by_base_job_id"
            -- Composite resources
            & T.replace "effects/{attribute}" "effect_attribute"
            -- Syntax
            & T.replace "/" "_"
            & T.replace "__" "_"
            & T.replace "__" "_"
            & T.replace "__" "_"
            & T.replace "__" "_"
