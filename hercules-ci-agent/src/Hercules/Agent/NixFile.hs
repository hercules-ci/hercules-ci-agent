{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Hercules.Agent.NixFile
  ( -- * Schemas
    HomeSchema,
    HerculesCISchema,
    OnPushSchema,
    OnScheduleSchema,
    ExtraInputsSchema,
    InputDeclSchema,
    InputsSchema,
    InputSchema,
    OutputsSchema,
    TimeConstraintsSchema,

    -- * Loading
    findNixFile,
    loadNixFile,
    HomeExpr (..),
    homeExprRawValue,
    getHerculesCI,
    loadDefaultHerculesCI,

    -- * @onPush@
    getVirtualValueByPath,
    parseExtraInputs,
  )
where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Map qualified as M
import Hercules.API.Agent.Evaluate.EvaluateEvent.InputDeclaration (InputDeclaration (SiblingInput), SiblingInput (MkSiblingInput))
import Hercules.API.Agent.Evaluate.EvaluateEvent.InputDeclaration qualified
import Hercules.Agent.NixFile.CiNixArgs (CiNixArgs (CiNixArgs))
import Hercules.Agent.NixFile.CiNixArgs qualified
import Hercules.Agent.NixFile.GitSource (GitSource)
import Hercules.Agent.NixFile.HerculesCIArgs (HerculesCIArgs)
import Hercules.Agent.NixFile.HerculesCIArgs qualified as HerculesCIArgs
import Hercules.CNix.Expr
  ( EvalState,
    Match (IsAttrs),
    NixAttrs,
    Value (Value, rtValue),
    addAllowedPath,
    assertType,
    autoCallFunction,
    evalFile,
    getAttr,
    getFlakeFromFlakeRef,
    getLocalFlake,
    match',
    toRawValue,
    unsafeAssertType,
    valueFromExpressionString,
  )
import Hercules.CNix.Expr.Raw (RawValue)
import Hercules.CNix.Expr.Schema (Attrs, Dictionary, MonadEval, PSObject (PSObject), Provenance (Other), StringWithoutContext, basicAttrsWithProvenance, dictionaryToMap, fromPSObject, toPSObject, (#.), (#?), ($?), (.$), (>>$.), type (->.), type (->?), type (.), type (::.), type (::?), type (::??), type (?), type (|.))
import Hercules.CNix.Expr.Schema qualified as Schema
import Hercules.Error (escalateAs)
import Paths_hercules_ci_agent (getDataFileName)
import Protolude hiding (evalState)
import System.Directory qualified as Dir
import System.FilePath (takeDirectory, takeFileName, (</>))
import UnliftIO.Directory (doesPathExist)

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
      -- NB This branch of logic is not used by hercules-ci-agent, which fetches
      --    directly from flakeref and does not go through a local path.
      --    An actual consumer of this branch is the hci CLI.

      -- TODO: Can Nix decide isGit (and more) for us?
      isGit <- doesPathExist (takeDirectory nixFile </> ".git")
      val <-
        liftIO
          ( if isGit
              then getFlakeFromFlakeRef evalState ("git+file://" <> encodeUtf8 (toS projectPath))
              else getLocalFlake evalState (toS projectPath)
          )
          >>= assertType evalState
      pure (Flake val)
    else do
      rootValueOrFunction <- liftIO $ evalFile evalState nixFile
      args <- unsafeAssertType @NixAttrs <$> liftIO (toRawValue evalState CiNixArgs {src = src})
      homeExpr <- liftIO $ autoCallFunction evalState rootValueOrFunction args
      pure (CiNix nixFile homeExpr)

getHomeExprObject :: (MonadEval m) => HomeExpr -> m (PSObject HomeSchema)
getHomeExprObject (Flake attrs) = pure PSObject {value = rtValue attrs, provenance = Schema.File "flake.nix"}
getHomeExprObject (CiNix f obj) = pure PSObject {value = obj, provenance = Schema.File f}

type HomeSchema = Attrs '["herculesCI" ::? Attrs '[] ->? HerculesCISchema]

type HerculesCISchema =
  Attrs
    '[ "onPush" ::? Dictionary OnPushSchema,
       "onSchedule" ::? Dictionary OnScheduleSchema
     ]

type OnPushSchema =
  Attrs
    '[ "extraInputs" ::? ExtraInputsSchema,
       "outputs" ::. OutputsFunction,
       "enable" ::? Bool
     ]

type OutputsFunction = InputsSchema ->? OutputsSchema

type ExtraInputsSchema = Dictionary InputDeclSchema

type InputDeclSchema =
  Attrs
    '[ "project" ::. StringWithoutContext,
       "ref" ::? StringWithoutContext
     ]

type OnScheduleSchema =
  Attrs
    '[ "extraInputs" ::? ExtraInputsSchema,
       "outputs" ::. OutputsFunction,
       "enable" ::? Bool,
       "when" ::?? TimeConstraintsSchema
     ]

type TimeConstraintsSchema =
  Attrs
    '[ "hour" ::?? HoursSchema,
       "minute" ::?? MinuteSchema,
       "dayOfWeek" ::?? DaysOfWeekSchema,
       "dayOfMonth" ::?? DaysOfMonthSchema
     ]

type HoursSchema = Int64 |. [Int64]

type MinuteSchema = Int64

type DaysOfWeekSchema = [StringWithoutContext]

type DaysOfMonthSchema = [Int64]

type InputsSchema = Dictionary InputSchema

type InputSchema = Dictionary RawValue

type OutputsSchema = Dictionary RawValue

type DefaultHerculesCIHelperSchema =
  Attrs
    '[ "addDefaults" ::. Attrs '[] ->. Attrs '[] ->. HerculesCISchema
     ]

exprString :: forall a m. (MonadEval m) => ByteString -> m (PSObject a)
exprString bs = do
  evalState <- ask
  value <- liftIO $ valueFromExpressionString evalState bs "/var/lib/empty/hercules-ci-agent-builtin"
  pure PSObject {value = value, provenance = Schema.Other "hercules-ci-agent built-in expression"}

getHerculesCI :: (MonadEval m) => HomeExpr -> HerculesCIArgs -> m (Maybe (PSObject HerculesCISchema))
getHerculesCI homeExpr args = do
  home <- getHomeExprObject homeExpr
  args' <- Schema.uncheckedCast <$> toPSObject args
  case homeExpr of
    CiNix {} ->
      home
        #? #herculesCI
        >>= traverse @Maybe \herculesCI ->
          herculesCI $? args'
    Flake flake ->
      Just <$> do
        -- fixup primaryRepo.outPath, which we didn't set to the right value for
        -- flakes earlier, because we don't have a local checkout.
        args'' <-
          exprString @(Attrs _ ->. HomeSchema ->. Attrs _)
            "args': flake: args' // { primaryRepo = args'.primaryRepo // { outPath = flake.outPath; }; }"
            >>$. pure args'
            >>$. pure home

        dh <- loadDefaultHerculesCI
        fn <- dh #. #addDefaults
        let flakeObj = basicAttrsWithProvenance flake $ Schema.Other "your flake"
        hci <- fn .$ flakeObj >>$. pure args''
        pure hci {Schema.provenance = Other "the herculesCI attribute of your flake (after adding defaults)"}

parseExtraInputs :: (MonadEval m) => PSObject ExtraInputsSchema -> m (Map ByteString InputDeclaration)
parseExtraInputs eis = dictionaryToMap eis >>= traverse parseInputDecl

parseInputDecl :: (MonadEval m) => PSObject InputDeclSchema -> m InputDeclaration
parseInputDecl d = do
  project <- d #. #project >>= fromPSObject
  ref <- d #? #ref >>= traverse fromPSObject
  pure $ SiblingInput $ MkSiblingInput {project = project, ref = ref}

-- | A function for retrieving values from `herculesCI` and legacy ci.nix.
-- It treats the expression as tree of attribute sets, making the required
-- function applications and context gather implicit.
--
-- For example, given a path, this will return the onPush output or legacy
-- ci.nix value. Oversimplifying:
--
-- @@@
-- e.g.  ["a" "b"]  => ((import file).herculesCI args).onPush.a.outputs.b
--       or falling back to
--       ["a" "b"]  => (import file legacyArgs).a.b
-- @@@
getVirtualValueByPath ::
  Ptr EvalState ->
  FilePath ->
  HerculesCIArgs ->
  -- | Resolve inputs to an attrset of fetched/fetchable stuff
  (Map ByteString InputDeclaration -> IO (Value NixAttrs)) ->
  [ByteString] ->
  IO (Maybe RawValue)
getVirtualValueByPath evalState filePath args resolveInputs attrPath = do
  homeExpr <- escalateAs FatalError =<< loadNixFile evalState filePath (HerculesCIArgs.primaryRepo args)
  flip runReaderT evalState do
    herculesCI <- getHerculesCI homeExpr args
    onPushMaybe <- join <$> for herculesCI \hci -> hci #? #onPush
    onScheduleMaybe <- join <$> for herculesCI \hci -> hci #? #onSchedule
    let require = MaybeT . pure
        m2s k (Just a) = M.singleton k a
        m2s _ Nothing = mempty

    runMaybeT do
      case attrPath of
        [] | isJust onPushMaybe || isJust onScheduleMaybe -> do
          onPush' <- traverse (liftIO . toRawValue evalState) onPushMaybe
          onSchedule' <- traverse (liftIO . toRawValue evalState) onScheduleMaybe
          liftIO $ toRawValue evalState (m2s ("onPush" :: ByteString) onPush' <> m2s "onSchedule" onSchedule')
        "onPush" : [] -> do
          Schema.value <$> require onPushMaybe
        "onPush" : jobName : attrPath' -> do
          onPush <- require onPushMaybe
          job <- MaybeT $ Schema.lookupDictBS jobName onPush
          outputs <- resolveAndInvokeOutputs job (liftIO . resolveInputs)
          outputAttrs <- Schema.check outputs
          MaybeT $ liftIO $ attrByPath evalState (rtValue outputAttrs) attrPath'
        "onSchedule" : [] -> do
          Schema.value <$> require onScheduleMaybe
        "onSchedule" : jobName : attrPath' -> do
          onSchedule <- require onScheduleMaybe
          job <- MaybeT $ Schema.lookupDictBS jobName onSchedule
          outputs <- resolveAndInvokeOutputs job (liftIO . resolveInputs)
          outputAttrs <- Schema.check outputs
          MaybeT $ liftIO $ attrByPath evalState (rtValue outputAttrs) attrPath'
        _ ->
          case onPushMaybe of
            Just jobs -> do
              case attrPath of
                [] -> pure $ Schema.value jobs -- Technically mapAttrs .outputs, meh
                (jobName : attrPath') -> do
                  Schema.lookupDictBS jobName jobs >>= \case
                    Just selectedJob -> do
                      outputs <- resolveAndInvokeOutputs selectedJob (liftIO . resolveInputs)
                      outputAttrs <- Schema.check outputs
                      MaybeT $ liftIO $ attrByPath evalState (rtValue outputAttrs) attrPath'
                    Nothing -> empty
            Nothing -> do
              MaybeT $ liftIO $ attrByPath evalState (homeExprRawValue homeExpr) attrPath

resolveAndInvokeOutputs ::
  ( MonadEval m,
    a . "outputs" ~ OutputsFunction,
    a ? "extraInputs" ~ ExtraInputsSchema
  ) =>
  PSObject (Attrs a) ->
  (Map ByteString InputDeclaration -> m (Value NixAttrs)) ->
  m (PSObject OutputsSchema)
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

loadDefaultHerculesCI :: (MonadEval m) => m (PSObject DefaultHerculesCIHelperSchema)
loadDefaultHerculesCI = do
  fname <- liftIO $ getDataFileName "data/default-herculesCI-for-flake.nix"
  evalState <- ask
  liftIO $ addAllowedPath evalState . encodeUtf8 . toS $ fname
  v <- liftIO $ evalFile evalState fname
  pure (PSObject {value = v, provenance = Other "<default herculesCI helper shim>"})
