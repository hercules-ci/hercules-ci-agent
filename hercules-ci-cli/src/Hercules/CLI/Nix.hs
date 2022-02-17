{-# LANGUAGE BlockArguments #-}

module Hercules.CLI.Nix where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Unlift (unliftIO)
import Data.Has (Has)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Hercules.API.Agent.Evaluate.EvaluateEvent.InputDeclaration (InputDeclaration (SiblingInput))
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.InputDeclaration as InputDeclaration
import qualified Hercules.API.Inputs.ImmutableGitInput as API.ImmutableGitInput
import Hercules.API.Projects (getJobSource)
import Hercules.Agent.NixFile (getOnPushOutputValueByPath)
import qualified Hercules.Agent.NixFile.GitSource as GitSource
import Hercules.Agent.NixFile.HerculesCIArgs (HerculesCIArgs)
import qualified Hercules.Agent.NixFile.HerculesCIArgs as HerculesCIArgs
import Hercules.CLI.Client (HerculesClientEnv, HerculesClientToken, determineDefaultApiBaseUrl, runHerculesClient)
import Hercules.CLI.Common (runAuthenticated)
import Hercules.CLI.Git (getGitRoot, getRef, getRev)
import Hercules.CLI.Options (scanOption)
import Hercules.CLI.Project (ProjectPath (projectPathProject), getProjectPath, projectPathReadM, projectResourceClientByPath)
import Hercules.CNix (Store)
import Hercules.CNix.Expr as Expr (EvalState, Match (IsAttrs), NixAttrs, RawValue, Value, getAttr, getAttrs, getFlakeFromGit, init, isDerivation, match', toValue, withEvalState, withStore)
import qualified Hercules.CNix.Util as CNix.Util
import qualified Hercules.CNix.Verbosity as CNix.Verbosity
import Options.Applicative as Optparse
import Options.Applicative.Types (unReadM)
import Protolude hiding (evalState)
import RIO (RIO)
import UnliftIO (MonadUnliftIO, UnliftIO (UnliftIO), askUnliftIO)

createHerculesCIArgs :: Maybe Text -> IO HerculesCIArgs
createHerculesCIArgs passedRef = do
  gitRoot <- getGitRoot
  gitRev <- getRev
  ref <- computeRef passedRef
  let gitSource = GitSource.fromRefRevPath ref gitRev (toS gitRoot)
  url <- determineDefaultApiBaseUrl
  pure $ HerculesCIArgs.fromGitSource gitSource HerculesCIArgs.HerculesCIMeta {apiBaseUrl = url}

computeRef :: Maybe Text -> IO Text
computeRef Nothing = getRef
computeRef (Just passedRef) = pure passedRef

resolveInputs ::
  (Has HerculesClientToken r, Has HerculesClientEnv r) =>
  UnliftIO (RIO r) ->
  Ptr EvalState ->
  Maybe ProjectPath ->
  Map ByteString InputDeclaration ->
  IO (Value NixAttrs)
resolveInputs uio evalState projectMaybe inputs = do
  projectPath <- unliftIO uio $ getProjectPath projectMaybe
  let resolveInput :: ByteString -> InputDeclaration -> IO RawValue
      resolveInput _name (SiblingInput input) = unliftIO uio do
        let resourceClient = projectResourceClientByPath (projectPath {projectPathProject = InputDeclaration.project input})
            jobNames = []
        immutableGitInput <- runHerculesClient (getJobSource resourceClient (InputDeclaration.ref input) jobNames)
        liftIO $ mkImmutableGitInputFlakeThunk evalState immutableGitInput
      resolveInput _name InputDeclaration.BogusInput {} = panic "resolveInput: not implemented yet"
  inputs
    & M.mapWithKey (,)
    & mapConcurrently (uncurry resolveInput)
    & (>>= toValue evalState)

refBranchToRef :: Maybe Text -> Maybe Text -> Maybe Text
refBranchToRef ref branch = ref <|> (("refs/heads/" <>) <$> branch)

withNix :: (MonadUnliftIO m) => (Store -> Ptr EvalState -> m b) -> m b
withNix f = do
  liftIO do
    Expr.init
    CNix.Util.installDefaultSigINTHandler
  UnliftIO uio <- askUnliftIO
  liftIO $ withStore \store -> withEvalState store (uio . f store)

ciNixAttributeCompleter :: Optparse.Completer
ciNixAttributeCompleter = mkTextCompleter \partial -> do
  withNix \_store evalState -> do
    CNix.Verbosity.setVerbosity CNix.Verbosity.Error
    ref <- do
      ref <- scanOption "--as-ref"
      branch <- scanOption "--as-branch"
      pure $ refBranchToRef ref branch
    projectMaybe <-
      scanOption "--project" <&> \maybeStr -> do
        s <- maybeStr
        rightToMaybe (runExcept (runReaderT (unReadM projectPathReadM) (toS s)))
    args <- createHerculesCIArgs ref
    let partialComponents = T.split (== '.') partial
        prefix = L.init partialComponents
        partialComponent = lastMay partialComponents & fromMaybe ""
        prefixStr = T.intercalate "." prefix
        addPrefix x = T.intercalate "." (prefix <> [x])
    runAuthenticated do
      uio <- askUnliftIO
      liftIO $
        getOnPushOutputValueByPath evalState (toS $ GitSource.outPath $ HerculesCIArgs.primaryRepo args) args (resolveInputs uio evalState projectMaybe) (encodeUtf8 <$> prefix) >>= \case
          Nothing -> pure []
          Just focusValue -> do
            match' evalState focusValue >>= \case
              IsAttrs attrset -> do
                attrs <- getAttrs attrset
                isDeriv <- isDerivation evalState focusValue
                if isDeriv
                  then pure [(mempty {Optparse.cioFiles = False}, prefixStr)]
                  else
                    let matches =
                          attrs
                            & M.keys
                            & map decodeUtf8
                            & filter (/= "recurseForDerivations")
                            & filter (T.isPrefixOf partialComponent)
                     in case matches of
                          [singleMatch] -> do
                            ma <- getAttr evalState attrset (encodeUtf8 singleMatch)
                            matchIsDeriv <-
                              ma & traverse (isDerivation evalState)
                                <&> fromMaybe False
                            if matchIsDeriv
                              then
                                pure $
                                  matches
                                    & map (\match -> (mempty {Optparse.cioAddSpace = True, Optparse.cioFiles = False}, addPrefix match))
                              else
                                pure $
                                  matches
                                    & map (\match -> (mempty {Optparse.cioAddSpace = False, Optparse.cioFiles = False}, addPrefix match <> "."))
                          _ ->
                            pure $
                              matches
                                & map (\match -> (mempty {Optparse.cioAddSpace = False, Optparse.cioFiles = False}, addPrefix match))
              _ -> pure []

attrByPath :: Ptr EvalState -> RawValue -> [ByteString] -> IO (Maybe RawValue)
attrByPath _ v [] = pure (Just v)
attrByPath evalState v (a : as) = do
  match' evalState v >>= \case
    IsAttrs attrs ->
      getAttr evalState attrs a
        >>= traverse (\attrValue -> attrByPath evalState attrValue as)
        & fmap join
    _ -> pure Nothing

mkTextCompleter :: (Text -> IO [(Optparse.CompletionItemOptions, Text)]) -> Completer
mkTextCompleter f = Optparse.mkCompleterWithOptions (fmap (fmap (uncurry CompletionItem . fmap toS)) . f . toS)

mkImmutableGitInputFlakeThunk :: Ptr EvalState -> API.ImmutableGitInput.ImmutableGitInput -> IO RawValue
mkImmutableGitInputFlakeThunk evalState git = do
  -- TODO: allow picking ssh/http url
  getFlakeFromGit
    evalState
    (API.ImmutableGitInput.httpURL git)
    (API.ImmutableGitInput.ref git)
    (API.ImmutableGitInput.rev git)
