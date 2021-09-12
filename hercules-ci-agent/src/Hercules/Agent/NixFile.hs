{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}

module Hercules.Agent.NixFile
  ( -- * Schemas
    HomeSchema,
    HerculesCISchema,
    OnPushSchema,
    ExtraInputsSchema,
    InputDeclSchema,
    InputsSchema,
    InputSchema,
    OutputsSchema,

    -- * Loading
    findNixFile,
    loadNixFile,
    HomeExpr (..),
    homeExprRawValue,
    getHerculesCI,

    -- * @onPush@
    getOnPushOutputValueByPath,
    parseExtraInputs,

    -- * Utilities
    computeArgsRequired,
  )
where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Hercules.API.Agent.Evaluate.EvaluateEvent.InputDeclaration (InputDeclaration (SiblingInput), SiblingInput (MkSiblingInput))
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.InputDeclaration
import Hercules.Agent.NixFile.CiNixArgs (CiNixArgs (CiNixArgs))
import qualified Hercules.Agent.NixFile.CiNixArgs
import Hercules.Agent.NixFile.GitSource (GitSource)
import Hercules.Agent.NixFile.HerculesCIArgs (HerculesCIArgs)
import qualified Hercules.Agent.NixFile.HerculesCIArgs as HerculesCIArgs
import Hercules.CNix.Expr
  ( EvalState,
    FunctionMatches (FunctionMatches),
    FunctionParams (FunctionParams),
    Match (IsAttrs),
    NixAttrs,
    NixString,
    Value (Value, rtValue),
    assertType,
    autoCallFunction,
    evalFile,
    getAttr,
    getLocalFlake,
    match',
    toRawValue,
    unsafeAssertType,
  )
import qualified Hercules.CNix.Expr as Expr
import Hercules.CNix.Expr.Raw (RawValue)
import Hercules.CNix.Expr.Schema (Attrs, Dictionary, MonadEval, PSObject (PSObject), dictionaryToMap, getText_, toPSObject, (#.), (#?), ($?), (>>$?), type (->?), type (::.), type (::?))
import qualified Hercules.CNix.Expr.Schema as Schema
import Hercules.Error (escalateAs)
import Protolude hiding (evalState)
import qualified System.Directory as Dir
import System.FilePath (takeFileName, (</>))

type Ambiguity = [FilePath]

searchPath :: [Ambiguity]
searchPath = [["nix/ci.nix", "ci.nix"], ["flake.nix"], ["default.nix"]]

findNixFile :: FilePath -> IO (Either Text FilePath)
findNixFile projectDir = do
  searchResult <-
    for searchPath $
      traverse $ \relPath ->
        let path = projectDir </> relPath
         in Dir.doesFileExist path >>= \case
              True -> pure $ Just (relPath, path)
              False -> pure Nothing
  pure $ case filter (not . null) $ map catMaybes searchResult of
    [(_relPath, unambiguous)] : _ -> pure unambiguous
    ambiguous : _ ->
      Left $
        "Don't know what to do, expecting only one of "
          <> Schema.englishOr (map (toS . fst) ambiguous)
    [] ->
      Left $
        "Please provide a Nix expression to build. Could not find any of "
          <> Schema.englishOr (concatMap (map toS) searchPath)
          <> " in your source"

-- | Expression containing the bulk of the project
data HomeExpr
  = Flake (Value NixAttrs)
  | CiNix FilePath RawValue

homeExprRawValue :: HomeExpr -> RawValue
homeExprRawValue (Flake (Value r)) = r
homeExprRawValue (CiNix _ r) = r

loadNixFile :: Ptr EvalState -> FilePath -> GitSource -> IO (Either Text HomeExpr)
loadNixFile evalState projectPath src = runExceptT do
  nixFile <- ExceptT $ findNixFile projectPath
  if takeFileName nixFile == "flake.nix"
    then do
      val <- liftIO $ getLocalFlake evalState (toS projectPath) >>= assertType evalState
      pure (Flake val)
    else do
      rootValueOrFunction <- liftIO $ evalFile evalState nixFile
      args <- unsafeAssertType @NixAttrs <$> liftIO (toRawValue evalState CiNixArgs {src = src})
      homeExpr <- liftIO $ autoCallFunction evalState rootValueOrFunction args
      pure (CiNix nixFile homeExpr)

getHomeExprObject :: MonadEval m => HomeExpr -> m (PSObject HomeSchema)
getHomeExprObject (Flake attrs) = pure PSObject {value = rtValue attrs, provenance = Schema.File "flake.nix"}
getHomeExprObject (CiNix f obj) = pure PSObject {value = obj, provenance = Schema.File f}

type HomeSchema = Attrs '["herculesCI" ::? Attrs '[] ->? HerculesCISchema]

type HerculesCISchema = Attrs '["onPush" ::? Dictionary OnPushSchema]

type OnPushSchema =
  Attrs
    '[ "extraInputs" ::? ExtraInputsSchema,
       "outputs" ::. InputsSchema ->? OutputsSchema
     ]

type ExtraInputsSchema = Dictionary InputDeclSchema

type InputDeclSchema = Attrs '["project" ::. NixString, "ref" ::? NixString]

type InputsSchema = Dictionary InputSchema

type InputSchema = Dictionary RawValue

type OutputsSchema = Dictionary RawValue

getHerculesCI :: MonadEval m => HomeExpr -> HerculesCIArgs -> m (Maybe (PSObject HerculesCISchema))
getHerculesCI homeExpr args = do
  home <- getHomeExprObject homeExpr
  attrVal <- home #? #herculesCI
  attrVal & traverse \herculesCI -> do
    pure herculesCI >>$? (Schema.uncheckedCast <$> toPSObject args)

data ArgsRequired
  = AllArgsRequired
  | SomeArgsRequired (Map ByteString Bool)

-- | Compute the arguments that are required to invoke a function, such that
-- any omissions are undetectable by the function.
computeArgsRequired :: FunctionParams -> ArgsRequired
computeArgsRequired = \case
  -- The lack of ellipsis means that the arguments can not contain anything not in the matches,
  -- so don't need to compute everything.
  FunctionParams
    { functionArgName = Just _,
      functionParamsMatches = Just FunctionMatches {functionMatchesEllipsis = False, functionMatches = matches}
    } -> SomeArgsRequired matches
  -- No name to access the unmatched args, so we don't have to compute everything.
  FunctionParams
    { functionArgName = Nothing,
      functionParamsMatches = Just FunctionMatches {functionMatches = matches}
    } -> SomeArgsRequired matches
  -- Catch-all: name and ellipsis, just name, primops
  _ -> AllArgsRequired

parseExtraInputs :: MonadEval m => PSObject ExtraInputsSchema -> m (Map ByteString InputDeclaration)
parseExtraInputs eis = dictionaryToMap eis >>= traverse parseInputDecl

parseInputDecl :: MonadEval m => PSObject InputDeclSchema -> m InputDeclaration
parseInputDecl d = do
  project <- d #. #project >>= getText_
  ref <- d #? #ref >>= traverse getText_
  pure $ SiblingInput $ MkSiblingInput {project = project, ref = ref}

-- | Given a path, return the onPush output or legacy ci.nix value
--
-- @@@
-- e.g.  ["a" "b"]  => ((import file).herculesCI args).onPush.a.outputs.b
--       or falling back to
--       ["a" "b"]  => (import file legacyArgs).a.b
-- @@@
getOnPushOutputValueByPath ::
  Ptr EvalState ->
  FilePath ->
  HerculesCIArgs ->
  -- | Resolve inputs to an attrset of fetched/fetchable stuff
  (Map ByteString InputDeclaration -> IO (Value NixAttrs)) ->
  [ByteString] ->
  IO (Maybe RawValue)
getOnPushOutputValueByPath evalState filePath args resolveInputs attrPath = do
  homeExpr <- escalateAs FatalError =<< loadNixFile evalState filePath (HerculesCIArgs.primaryRepo args)
  onPush <- flip runReaderT evalState $ runMaybeT do
    herculesCI <- MaybeT $ getHerculesCI homeExpr args
    MaybeT $ herculesCI #? #onPush

  -- No backtracking. It's either legacy or not...
  case onPush of
    Just jobs -> flip runReaderT evalState $ do
      case attrPath of
        [] -> pure $ Just $ Schema.value jobs -- Technically mapAttrs .outputs, meh
        (jobName : attrPath') -> do
          Schema.lookupDictBS jobName jobs >>= \case
            Just selectedJob -> do
              outputs <- resolveAndInvokeOutputs selectedJob (liftIO . resolveInputs)
              outputAttrs <- Schema.check outputs
              liftIO $ attrByPath evalState (rtValue outputAttrs) attrPath'
            Nothing -> pure Nothing
    Nothing -> do
      attrByPath evalState (homeExprRawValue homeExpr) attrPath

resolveAndInvokeOutputs :: MonadEval m => PSObject OnPushSchema -> (Map ByteString InputDeclaration -> m (Value NixAttrs)) -> m (PSObject OutputsSchema)
resolveAndInvokeOutputs job resolveInputs = do
  inputs <- job #? #extraInputs >>= traverse parseExtraInputs
  resolved <- resolveInputs (fromMaybe mempty inputs)
  f <- job #. #outputs
  f $? (PSObject {provenance = Schema.Data, value = rtValue resolved})

attrByPath :: Ptr EvalState -> RawValue -> [ByteString] -> IO (Maybe RawValue)
attrByPath _ v [] = pure (Just v)
attrByPath evalState v (a : as) = do
  match' evalState v >>= \case
    IsAttrs attrs ->
      getAttr evalState attrs a
        >>= traverse (\attrValue -> attrByPath evalState attrValue as)
        & fmap join
    _ -> pure Nothing
