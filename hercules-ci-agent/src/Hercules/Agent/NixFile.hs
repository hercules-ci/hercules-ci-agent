{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Hercules.Agent.NixFile
  ( findNixFile,
    loadNixFile,

    -- * Facade functions
    getOnPushOutputValueByPath,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import qualified Data.Map as M
import Hercules.Agent.NixFile.CiNixArgs (CiNixArgs (CiNixArgs))
import qualified Hercules.Agent.NixFile.CiNixArgs
import Hercules.Agent.NixFile.GitSource (GitSource)
import Hercules.Agent.NixFile.HerculesCIArgs (HerculesCIArgs)
import qualified Hercules.Agent.NixFile.HerculesCIArgs as HerculesCIArgs
import Hercules.CNix.Expr
  ( EvalState,
    FunctionMatches (FunctionMatches),
    FunctionParams (FunctionParams),
    Match (IsAttrs, IsFunction),
    NixAttrs,
    Value (Value, rtValue),
    apply,
    assertType,
    autoCallFunction,
    checkType,
    evalFile,
    functionParams,
    getAttr,
    getAttrs,
    getLocalFlake,
    match',
    toRawValue,
    unsafeAssertType,
  )
import qualified Hercules.CNix.Expr as Expr
import Hercules.CNix.Expr.Raw (RawValue)
import Hercules.Error (escalateAs)
import Protolude hiding (evalState)
import qualified System.Directory as Dir
import System.FilePath (takeFileName, (</>))

type Ambiguity = [FilePath]

searchPath :: [Ambiguity]
searchPath = [["flake.nix"], ["nix/ci.nix", "ci.nix"], ["default.nix"]]

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
          <> englishConjunction "or" (map fst ambiguous)
    [] ->
      Left $
        "Please provide a Nix expression to build. Could not find any of "
          <> englishConjunction "or" (concat searchPath)
          <> " in your source"

englishConjunction :: Show a => Text -> [a] -> Text
englishConjunction _ [] = "none"
englishConjunction _ [a] = show a
englishConjunction connective [a1, a2] =
  show a1 <> " " <> connective <> " " <> show a2
englishConjunction connective (a : as) =
  show a <> ", " <> englishConjunction connective as

-- | Expression containing the bulk of the project
data HomeExpr
  = Flake (Value NixAttrs)
  | CiNix RawValue

homeExprRawValue :: HomeExpr -> RawValue
homeExprRawValue (Flake (Value r)) = r
homeExprRawValue (CiNix r) = r

loadNixFile :: Ptr EvalState -> FilePath -> GitSource -> IO (Either Text HomeExpr)
loadNixFile evalState projectPath src = runExceptT do
  nixFile <- ExceptT $ findNixFile projectPath
  if takeFileName nixFile == "flake.nix"
    then do
      val <- liftIO $ getLocalFlake evalState (toS projectPath)
      pure (Flake val)
    else do
      rootValueOrFunction <- liftIO $ evalFile evalState nixFile
      args <- unsafeAssertType @NixAttrs <$> liftIO (toRawValue evalState CiNixArgs {src = src})
      homeExpr <- liftIO $ autoCallFunction evalState rootValueOrFunction args
      pure (CiNix homeExpr)

getHomeExprAttr :: Ptr EvalState -> HomeExpr -> ByteString -> IO (Maybe RawValue)
getHomeExprAttr evalState (Flake attrs) name = getAttr evalState attrs name
getHomeExprAttr evalState (CiNix val) name = runMaybeT do
  attrs <-
    liftIO (match' evalState val) >>= \case
      IsAttrs a -> pure a
      _ -> empty
  MaybeT $ getAttr evalState attrs name

getHerculesCI :: Ptr EvalState -> HomeExpr -> HerculesCIArgs -> IO (Maybe (Value NixAttrs))
getHerculesCI evalState homeExpr args = runMaybeT do
  val <- MaybeT $ getHomeExprAttr evalState homeExpr "herculesCI"
  liftIO (match' evalState val) >>= \case
    IsAttrs a -> pure a
    IsFunction f -> do
      herculesCIArgs <- lift $ toRawValue evalState args
      herculesCIVal <- lift $ apply evalState (rtValue f) herculesCIArgs
      castAttrs evalState herculesCIVal
    _ -> empty

castAttrs ::
  (MonadIO m, Alternative m) =>
  Ptr EvalState ->
  RawValue ->
  m (Value NixAttrs)
castAttrs evalState val = do
  liftIO (match' evalState val) >>= \case
    IsAttrs a -> pure a
    _ -> empty

getOnPushAttrs :: Ptr EvalState -> Value NixAttrs -> IO (Maybe (Value NixAttrs))
getOnPushAttrs evalState herculesCI = do
  runMaybeT do
    onPushVal <- MaybeT $ getAttr evalState herculesCI "onPush"
    castAttrs evalState onPushVal

data InputsRequired
  = AllInputsRequired
  | SomeInputsRequired (Map ByteString Bool)

-- | Compute the arguments that are required to invoke a function, such that
-- any omissions are undetectable by the function.
computeArgsMissing :: FunctionParams -> InputsRequired
computeArgsMissing = \case
  -- The lack of ellipsis means that the arguments can not contain anything not in the matches,
  -- so don't need to compute everything.
  FunctionParams
    { functionArgName = Just _,
      functionParamsMatches = Just (FunctionMatches {functionMatchesEllipsis = False, functionMatches = matches})
    } -> SomeInputsRequired matches
  -- No name to access the unmatched args, so we don't have to compute everything.
  FunctionParams
    { functionArgName = Nothing,
      functionParamsMatches = Just (FunctionMatches {functionMatches = matches})
    } -> SomeInputsRequired matches
  -- Catch-all: name and ellipsis, just name, primops
  _ -> AllInputsRequired

getInputs :: Ptr EvalState -> Value NixAttrs -> IO (Map ByteString RawValue)
getInputs evalState onPush = do
  rawExtraInputs <- getAttr evalState onPush "extraInputs"
  extraInputs <- case rawExtraInputs of
    Nothing -> pure mempty
    Just x -> do
      checkType evalState x >>= \case
        Nothing -> panic "herculesCI{}.onPush.<name>.extraInputs must be an attrset"
        Just attrs -> getAttrs attrs
  extraInputs
    & M.mapWithKey (,)
    & mapConcurrently (uncurry (fetchMutableInput evalState))

-- assertType evalState =<< toRawValue evalState (inputs :: Map ByteString RawValue)

fetchMutableInput :: Ptr EvalState -> ByteString -> RawValue -> IO RawValue
fetchMutableInput = panic "fetchMutableInput not implemented yet"

invokeOutputs :: Ptr EvalState -> Value NixAttrs -> IO (Maybe (Value NixAttrs))
invokeOutputs evalState onPush = runMaybeT do
  outputsVal <- MaybeT $ getAttr evalState onPush "outputs"
  lift $
    match' evalState outputsVal >>= \case
      IsAttrs attrs -> pure attrs
      IsFunction fn -> do
        params <- functionParams evalState fn
        inputs <- getInputs evalState onPush
        let fixedArgs = mempty
            (//) = flip M.union -- make it like Nix
            argsMap = fixedArgs // inputs
        args <- assertType evalState =<< toRawValue evalState argsMap
        rawOutputs <- autoCallFunction evalState (rtValue fn) args
        checkType evalState rawOutputs >>= \case
          Nothing -> panic "herculesCI{}.onPush.<name>.outputs{...} must return an attribute set."
          Just attrs -> pure attrs

      -- case params of
      --   SomeInputsRequired params | M.keysSet `S.isSubsetOf` xyz ->
      --   AllInputsRequired -> panic "inputs not implemented yet"
      --    -> panic "inputs not implemented yet"
      _ ->
        panic "herculesCI{}.onPush.<name>.outputs must be an attribute set or function returning an attribute set."

-- | Given a path, return the onPush output or legacy ci.nix value
--
-- @@@
-- e.g.  ["a" "b"]  => ((import file).herculesCI args).onPush.a.outputs.b
--       or falling back to
--       ["a" "b"]  => (import file legacyArgs).a.b
-- @@@
getOnPushOutputValueByPath :: Ptr EvalState -> FilePath -> HerculesCIArgs -> [ByteString] -> IO (Maybe RawValue)
getOnPushOutputValueByPath evalState filePath args attrPath = do
  homeExpr <- escalateAs FatalError =<< loadNixFile evalState filePath (HerculesCIArgs.primaryRepo args)
  onPush <- runMaybeT do
    herculesCI <- MaybeT $ getHerculesCI evalState homeExpr args
    MaybeT $ getOnPushAttrs evalState herculesCI

  -- No backtracking. It's legacy or not...
  -- TODO: warn when using the legacy behavior. ci.nix can also use the herculesCI attr.
  case onPush of
    Just jobs -> do
      case attrPath of
        [] -> pure $ Just $ rtValue jobs -- Technically mapAttrs .outputs, meh
        (jobName : attrPath') -> do
          jobsMap <- getAttrs jobs
          case M.lookup jobName jobsMap of
            Just selectedJob -> do
              -- TODO: call `outputs`
              -- attrByPath evalState selectedJob ("outputs" : attrPath')
              selectedJobAttrs <-
                checkType evalState selectedJob
                  >>= maybe (panic "onPush.<name> must be an attribute set, containing an outputs attribute.") pure
              outputs <- invokeOutputs evalState selectedJobAttrs
              case outputs of
                Nothing -> pure Nothing
                Just outs -> attrByPath evalState (rtValue outs) attrPath'
            Nothing -> pure Nothing
    Nothing -> do
      attrByPath evalState (homeExprRawValue homeExpr) attrPath

attrByPath :: Ptr EvalState -> RawValue -> [ByteString] -> IO (Maybe RawValue)
attrByPath _ v [] = pure (Just v)
attrByPath evalState v (a : as) = do
  match' evalState v >>= \case
    IsAttrs attrs ->
      getAttr evalState attrs a
        >>= traverse (\attrValue -> attrByPath evalState attrValue as)
        & fmap join
    _ -> pure Nothing
