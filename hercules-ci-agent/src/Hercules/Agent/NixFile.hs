{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Hercules.Agent.NixFile
  ( findNixFile,
    loadNixFile,
    getExtraInputs,
    getOnPushOutputValueByPath,

    -- * Utilities
    computeArgsRequired,
    -- TODO: move
    InputDeclaration (..),
    SiblingInput (..),
  )
where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Coerce (coerce)
import qualified Data.Map as M
import qualified Data.Set as S
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
    getAttr,
    getAttrs,
    getLocalFlake,
    getStringIgnoreContext,
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
      val <- liftIO $ getLocalFlake evalState (toS projectPath) >>= assertType evalState
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

newtype HerculesCIAttrs = HerculesCIAttrs (Value NixAttrs)

getHerculesCI :: Ptr EvalState -> HomeExpr -> HerculesCIArgs -> IO (Maybe HerculesCIAttrs)
getHerculesCI evalState homeExpr args = runMaybeT do
  val <- MaybeT $ getHomeExprAttr evalState homeExpr "herculesCI"
  liftIO (match' evalState val) >>= \case
    IsAttrs a -> pure $ HerculesCIAttrs a
    IsFunction f -> do
      herculesCIArgs <- lift $ toRawValue evalState args
      herculesCIVal <- lift $ apply (rtValue f) herculesCIArgs
      HerculesCIAttrs <$> castAttrs evalState herculesCIVal
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

newtype JobAttrs = JobAttrs (Value NixAttrs)

getOnPushAttrs :: Ptr EvalState -> HerculesCIAttrs -> IO (Maybe (Value NixAttrs))
getOnPushAttrs evalState (HerculesCIAttrs herculesCI) = do
  runMaybeT do
    onPushVal <- MaybeT $ getAttr evalState herculesCI "onPush"
    castAttrs evalState onPushVal

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

getExtraInputs :: Ptr EvalState -> JobAttrs -> IO (Map ByteString InputDeclaration)
getExtraInputs evalState (JobAttrs jobAttrs) = do
  rawExtraInputs <- getAttr evalState jobAttrs "extraInputs"
  case rawExtraInputs of
    Nothing -> pure mempty
    Just x -> do
      checkType evalState x >>= \case
        Nothing -> panic "herculesCI{}.onPush.<name>.extraInputs must be an attrset"
        Just inputsAttrs ->
          getAttrs inputsAttrs >>= traverse \v -> do
            input <- checkType evalState v >>= maybe (panic "herculesCI{}.onPush.<name>.extraInputs.<name> must be an attrset") pure
            inputAttrs <- getAttrs input
            let keys = M.keysSet inputAttrs
            if keys == S.fromList ["project"]
              || keys == S.fromList ["project", "ref"]
              then do
                let projectValue = fromMaybe (panic "repo attr can't disappear") $ M.lookup "project" inputAttrs
                projectStr <- assertType evalState projectValue >>= getStringIgnoreContext
                refStr <- for (M.lookup "ref" inputAttrs) (assertType evalState >=> getStringIgnoreContext)
                pure $ SiblingInput $ MkSiblingInput {project = decodeUtf8With lenientDecode projectStr, ref = decodeUtf8With lenientDecode <$> refStr}
              else panic "Did not recognize herculesCI{}.onPush.<name>.extraInputs.<name> keys."

data InputDeclaration = SiblingInput SiblingInput

data SiblingInput = MkSiblingInput
  { project :: Text,
    ref :: Maybe Text
  }

invokeOutputs :: Ptr EvalState -> JobAttrs -> Value NixAttrs -> IO (Maybe (Value NixAttrs))
invokeOutputs evalState (JobAttrs jobAttrs) args = runMaybeT do
  outputsVal <- MaybeT $ getAttr evalState jobAttrs "outputs"
  lift $
    match' evalState outputsVal >>= \case
      IsAttrs attrs -> pure attrs
      IsFunction fn -> do
        rawOutputs <- autoCallFunction evalState (rtValue fn) args
        checkType evalState rawOutputs >>= \case
          Nothing -> panic "herculesCI{}.onPush.<name>.outputs{...} must return an attribute set."
          Just attrs -> pure attrs
      _ ->
        panic "herculesCI{}.onPush.<name>.outputs must be an attribute set or function returning an attribute set."

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
  onPush <- runMaybeT do
    herculesCI <- MaybeT $ getHerculesCI evalState homeExpr args
    MaybeT $ getOnPushAttrs evalState herculesCI

  -- No backtracking. It's legacy or not...
  -- TODO: warn when using the legacy behavior. ci.nix can also use the herculesCI attr.
  case onPush of
    Just jobs -> do
      case attrPath of
        [] -> pure $ Just $ coerce jobs -- Technically mapAttrs .outputs, meh
        (jobName : attrPath') -> do
          jobsMap <- getAttrs (coerce jobs)
          case M.lookup jobName jobsMap of
            Just selectedJob -> do
              -- TODO: call `outputs`
              -- attrByPath evalState selectedJob ("outputs" : attrPath')
              selectedJobAttrs <-
                checkType evalState selectedJob
                  >>= maybe (panic "onPush.<name> must be an attribute set, containing an outputs attribute.") (pure . JobAttrs)
              inputs <- getExtraInputs evalState selectedJobAttrs
              resolved <- resolveInputs inputs
              outputs <- invokeOutputs evalState selectedJobAttrs resolved
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
