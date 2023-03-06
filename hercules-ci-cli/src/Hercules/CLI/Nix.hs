{-# LANGUAGE BlockArguments #-}

module Hercules.CLI.Nix where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Unlift (unliftIO)
import Data.Has (Has)
import qualified Data.Map as M
import qualified Data.Text as T
import Hercules.API.Agent.Evaluate.EvaluateEvent.InputDeclaration (InputDeclaration (SiblingInput))
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.InputDeclaration as InputDeclaration
import Hercules.API.Attribute (attributePathFromString, attributePathToString)
import qualified Hercules.API.Inputs.ImmutableGitInput as API.ImmutableGitInput
import Hercules.API.Projects (getJobSource)
import Hercules.Agent.NixFile (getVirtualValueByPath)
import qualified Hercules.Agent.NixFile.GitSource as GitSource
import Hercules.Agent.NixFile.HerculesCIArgs (CISystems (CISystems), HerculesCIArgs)
import qualified Hercules.Agent.NixFile.HerculesCIArgs as HerculesCIArgs
import Hercules.CLI.Client (HerculesClientEnv, HerculesClientToken, determineDefaultApiBaseUrl, runHerculesClient)
import Hercules.CLI.Common (runAuthenticated)
import Hercules.CLI.Git (getGitRoot, getRef, getRev, getUpstreamURL, guessForgeTypeFromURL)
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
  upstreamURL <- getUpstreamURL
  let remoteHttpUrl = upstreamURL <$ guard ("http" `T.isPrefixOf` upstreamURL)
      remoteSshUrl = upstreamURL <$ guard (not ("http" `T.isPrefixOf` upstreamURL))
      guessWebUrlFromHttpUrl url = T.stripSuffix ".git" url & fromMaybe url
  let gitSource =
        GitSource.GitSource
          { outPath = toS gitRoot,
            ref = ref,
            rev = gitRev,
            shortRev = GitSource.shortRevFromRev gitRev,
            branch = GitSource.branchFromRef ref,
            tag = GitSource.tagFromRef ref,
            remoteHttpUrl = remoteHttpUrl,
            remoteSshUrl = remoteSshUrl,
            webUrl = guessWebUrlFromHttpUrl <$> remoteHttpUrl,
            forgeType = guessForgeTypeFromURL upstreamURL,
            owner = Nothing {- TODO; agent only for now -},
            name = Nothing {- TODO; agent only for now -}
          }
  url <- determineDefaultApiBaseUrl
  pure $ HerculesCIArgs.fromGitSource gitSource HerculesCIArgs.HerculesCIMeta {apiBaseUrl = url, ciSystems = CISystems Nothing}

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

mkSemanticTextCompleter :: (Text -> IO [(CompletionItemOptions, Text)]) -> Completer
mkSemanticTextCompleter f =
  mkTextCompleter
    ( \input -> do
        let startsEscape = input & T.reverse & T.takeWhile (== '\\') & T.length & odd
            innerCompleter = isoCompleter decodeBash encodeBash f
        if startsEscape
          then do
            r <- innerCompleter (T.dropEnd 1 input)
            -- Requiring input to be a prefix of the suggestions prevents corrections,
            -- so we only filter when necessary.
            pure
              [ item
                | item@(_, suggestionText) <- r,
                  input `T.isPrefixOf` suggestionText
              ]
          else innerCompleter input
    )

mkAttributePathCompleter :: (([Text], Text) -> IO [(CompletionItemOptions, ([Text], Bool))]) -> Completer
mkAttributePathCompleter f =
  mkSemanticTextCompleter
    ( \input -> do
        let startsEscape =
              (input & T.reverse & T.takeWhile (== '\\') & T.length & odd)
                || (".\"" `T.isSuffixOf` input)
            decode s
              | ".\"\"" `T.isSuffixOf` s =
                  (attributePathFromString s, "")
            decode s =
              let path = attributePathFromString s
               in (initSafe path, lastMay path & fromMaybe "")
            encode (path, dot) = attributePathToString path <> if dot then "." else ""
            innerCompleter = nestedCompleter decode encode f
        if startsEscape
          then do
            r <- innerCompleter (T.dropEnd 1 input)
            -- Requiring input to be a prefix of the suggestions prevents corrections,
            -- so we only filter when necessary.
            pure
              [ item
                | item@(_, suggestionText) <- r,
                  input `T.isPrefixOf` suggestionText
              ]
          else innerCompleter input
    )

isoCompleter :: (b -> a) -> (a -> b) -> (a -> IO [(CompletionItemOptions, a)]) -> (b -> IO [(CompletionItemOptions, b)])
isoCompleter = nestedCompleter

nestedCompleter :: (a -> b) -> (c -> d) -> (b -> IO [(CompletionItemOptions, c)]) -> (a -> IO [(CompletionItemOptions, d)])
nestedCompleter parse unparse f = fmap (fmap (fmap unparse)) . f . parse

encodeBash :: Text -> Text
encodeBash = toS . f . toS
  where
    f ('"' : s) = '\\' : '"' : f s
    f ('\'' : s) = '\\' : '\'' : f s
    f ('\\' : s) = '\\' : '\\' : f s
    f (' ' : s) = '\\' : ' ' : f s
    f (c : s) = c : f s
    f "" = ""

decodeBash :: Text -> Text
decodeBash = toS . g . toS
  where
    g ('\\' : c : s) = c : g s
    g (c : s) = c : g s
    g "" = ""

ciNixAttributeCompleter :: Optparse.Completer
ciNixAttributeCompleter = mkAttributePathCompleter \(partialPath, partialComponent) -> do
  withNix \_store evalState -> do
    CNix.Verbosity.setVerbosity CNix.Verbosity.Error
    ref <- do
      ref <- liftA2 (<|>) (scanOption "--as-ref") (scanOption "--pretend-ref")
      branch <- liftA2 (<|>) (scanOption "--as-branch") (scanOption "--pretend-branch")
      pure $ refBranchToRef ref branch
    projectMaybe <-
      scanOption "--project" <&> \maybeStr -> do
        s <- maybeStr
        rightToMaybe (runExcept (runReaderT (unReadM projectPathReadM) (toS s)))
    args <- createHerculesCIArgs ref
    runAuthenticated do
      uio <- askUnliftIO
      liftIO $
        getVirtualValueByPath evalState (toS $ GitSource.outPath $ HerculesCIArgs.primaryRepo args) args (resolveInputs uio evalState projectMaybe) (encodeUtf8 <$> partialPath) >>= \case
          Nothing -> pure []
          Just focusValue -> do
            match' evalState focusValue >>= \case
              IsAttrs attrset -> do
                attrs <- getAttrs evalState attrset
                isDeriv <- isDerivation evalState focusValue
                if isDeriv
                  then pure [(mempty {Optparse.cioFiles = False}, (partialPath, False))]
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
                              ma
                                & traverse (isDerivation evalState)
                                <&> fromMaybe False
                            pure $
                              matches
                                & map (\match -> (mempty {Optparse.cioAddSpace = matchIsDeriv, Optparse.cioFiles = False}, (partialPath ++ [match], not matchIsDeriv)))
                          _ ->
                            pure $
                              matches
                                & map (\match -> (mempty {Optparse.cioAddSpace = False, Optparse.cioFiles = False}, (partialPath ++ [match], False)))
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
