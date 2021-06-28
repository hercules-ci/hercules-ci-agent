{-# LANGUAGE BlockArguments #-}

module Hercules.CLI.Nix where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Hercules.Agent.NixFile (findNixFile)
import Hercules.CLI.Exception (UserException (UserException))
import Hercules.CLI.Git (getGitRoot, getRef, getRev)
import Hercules.CLI.Options (scanOption)
import Hercules.CNix (Store)
import Hercules.CNix.Expr as Expr (EvalState, Match (IsAttrs), RawValue, autoCallFunction, evalArgs, evalFile, getAttr, getAttrs, init, isDerivation, match', withEvalState, withStore)
import qualified Hercules.CNix.Util as CNix.Util
import Hercules.Error (escalateAs)
import Options.Applicative as Optparse
import Protolude hiding (evalState)
import UnliftIO (MonadUnliftIO, UnliftIO (UnliftIO), askUnliftIO)

callCiNix :: Ptr EvalState -> Maybe Text -> IO (FilePath, RawValue)
callCiNix evalState passedRef = do
  gitRoot <- getGitRoot
  gitRev <- getRev
  gitRef <- getRef
  nixFile <- findNixFile gitRoot >>= escalateAs UserException
  let ref = fromMaybe gitRef passedRef
  args <- evalArgs evalState ["--arg", "src", "{ ref = ''" <> encodeUtf8 ref <> "''; rev = ''" <> encodeUtf8 gitRev <> "''; outPath = ''" <> encodeUtf8 (toS gitRoot) <> "''; }"]
  rootValueOrFunction <- evalFile evalState nixFile
  (nixFile,) <$> autoCallFunction evalState rootValueOrFunction args

refBranchToRef :: Maybe Text -> Maybe Text -> Maybe Text
refBranchToRef ref branch = ref <|> (("refs/heads/" <>) <$> branch)

withNix :: (MonadUnliftIO m) => (Store -> Ptr EvalState -> m b) -> m b
withNix f = do
  liftIO do
    Expr.init
    CNix.Util.installDefaultSigINTHandler
  UnliftIO unliftIO <- askUnliftIO
  liftIO $ withStore \store -> withEvalState store (unliftIO . f store)

ciNixAttributeCompleter :: Optparse.Completer
ciNixAttributeCompleter = mkTextCompleter \partial -> do
  withNix \_store evalState -> do
    ref <- do
      ref <- scanOption "--as-ref"
      branch <- scanOption "--as-branch"
      pure $ refBranchToRef ref branch
    (_, rootValue) <- callCiNix evalState ref
    let partialComponents = T.split (== '.') partial
        prefix = L.init partialComponents
        partialComponent = lastMay partialComponents & fromMaybe ""
        prefixStr = T.intercalate "." prefix
        addPrefix x = T.intercalate "." (prefix <> [x])
    attrByPath evalState rootValue (encodeUtf8 <$> prefix) >>= \case
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
