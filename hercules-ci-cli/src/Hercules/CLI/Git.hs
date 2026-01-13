{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.CLI.Git where

import Data.List (dropWhileEnd)
import qualified Data.Text as T
import Hercules.CLI.Exception (exitMsg)
import Network.URI (URI (uriAuthority), URIAuth (uriRegName), parseURI)
import Protolude
import System.Directory (doesDirectoryExist)
import System.Process (readProcess)

readProcessString :: FilePath -> [[Char]] -> [Char] -> IO [Char]
readProcessString exe args input = dropWhileEnd (== '\n') <$> readProcess exe args input

readProcessItem :: FilePath -> [[Char]] -> [Char] -> IO Text
readProcessItem exe args input = toS <$> readProcessString exe args input

getGitRoot :: IO FilePath
getGitRoot = do
  p <- readProcessString "git" ["rev-parse", "--show-toplevel"] mempty
  unlessM (doesDirectoryExist p) $ panic $ "git root `" <> toS p <> "` is not a directory?"
  pure p

getRemotes :: IO [Text]
getRemotes = readProcess "git" ["remote"] mempty <&> toS <&> lines <&> filter (/= "")

getRef :: IO Text
getRef = do
  readProcessItem "git" ["rev-parse", "--symbolic-full-name", "HEAD"] mempty

getRev :: IO Text
getRev = do
  readProcessItem "git" ["rev-parse", "HEAD"] mempty

-- | rev (sha) and ref
getRevsAndRefs :: IO [(Text, Text)]
getRevsAndRefs =
  -- restrict to heads and tags, because other ones aren't relevant on CI, probably
  readProcess "git" ["show-ref"] mempty <&> \x ->
    x
      & toS
      & T.lines
      & map \ln ->
        ln
          & T.break isSpace
          & fmap (T.dropWhile isSpace)

getRefs :: IO [Text]
getRefs = getRevsAndRefs <&> map snd

getHypotheticalRefs :: IO [Text]
getHypotheticalRefs = do
  refs <- getRefs
  pure $ sort $ ordNub (refs <> map ("refs/heads/" <>) (allBranches refs) <> map ("refs/tags/" <>) (allTags refs))

allBranches :: [Text] -> [Text]
allBranches = concatMap filterRef
  where
    filterRef ref =
      toList (T.stripPrefix "refs/heads/" ref)
        <|> T.dropWhile (== '/') . T.dropWhile (/= '/') <$> toList (T.stripPrefix "refs/remotes/" ref)

getAllBranches :: IO [Text]
getAllBranches = getRefs <&> allBranches

allTags :: [Text] -> [Text]
allTags = mapMaybe (T.stripPrefix "refs/tags/")

getAllTags :: IO [Text]
getAllTags = getRefs <&> allTags

getUpstreamURL :: IO Text
getUpstreamURL = do
  remotes <- getRemotes
  case remotes of
    [x] -> getRemoteURL x
    _ -> do
      (getBranchUpstream >>= getRemoteURL) `onException` do
        putErrText "hci: could not determine git upstream repository url"

getUpstreamRef :: IO Text
getUpstreamRef =
  toS <$> readProcessString "git" ["rev-parse", "--symbolic-full-name", "@{u}"] mempty

getBranchUpstream :: IO Text
getBranchUpstream = do
  upstreamRef <-
    readProcessString "git" ["rev-parse", "--symbolic-full-name", "@{u}"] mempty
      `onException` putErrText "hci: could not determine current branch's upstream"
  let refsRemotes = "refs/remotes/"
  if refsRemotes `isPrefixOf` upstreamRef
    then pure $ toS $ takeWhile (/= '/') $ drop (length refsRemotes) upstreamRef
    else do
      exitMsg "upstream branch is not remote"

getCurrentBranchMaybe :: IO (Maybe Text)
getCurrentBranchMaybe =
  do
    x <- readProcessString "git" ["rev-parse", "--symbolic-full-name", "HEAD"] mempty

    case x of
      'r' : 'e' : 'f' : 's' : '/' : 'h' : 'e' : 'a' : 'd' : 's' : '/' : branch -> pure (Just (toS branch))
      _ -> pure Nothing
    `catch` \(_e :: SomeException) ->
      pure Nothing

getIsDefault :: IO Bool
getIsDefault = do
  try getUpstreamRef >>= \case
    Right _upstreamRef ->
      do
        upstream <- getBranchUpstream
        upstreamRef <- readProcessString "git" ["rev-parse", "--symbolic-full-name", "@{u}"] mempty
        upstreamDefaultRef <-
          readProcessString "git" ["rev-parse", "--symbolic-full-name", toS upstream <> "/HEAD"] mempty
            `onException` do
              putErrText "hci: Could not determine remote default branch"
              putErrText "     This may happen when the repository was initialized with git init instead of git clone"
              putErrText "     It can usually be fixed by running:"
              putErrText "         git remote set-head origin -a"
        pure (upstreamRef == upstreamDefaultRef)
        `onException` putErrText "hci: could not determine whether branch matches default branch"
    Left (_ :: SomeException) -> do
      getCurrentBranchMaybe >>= \case
        Nothing ->
          exitMsg "hci: Can't infer the context of your effect when you're on a git detached head."
        Just x -> do
          remotes <- getRemotes
          case remotes of
            [upstream] -> do
              putErrText "hci: Your branch does not seem to have an upstream. Assuming the local branch name and the single remote."
              upstreamDefaultRef <- readProcessString "git" ["rev-parse", "--symbolic-full-name", toS upstream <> "/HEAD"] mempty
              pure $ upstreamDefaultRef == "refs/heads/" ++ toS x
            [] ->
              exitMsg "hci: Can't infer whether you're on the default branch, because the repository does not have a remote."
            _multiple ->
              exitMsg "hci: Can't infer whether you're on the default branch, because the current branch does not have an upstream, and multiple remotes exist. Please set the upstream for the current branch."

getRemoteURL :: Text -> IO Text
getRemoteURL remoteName =
  readProcessItem "git" ["remote", "get-url", toS remoteName] mempty

-- TODO: store forge type in credentials.json
guessForgeTypeFromURL :: Text -> Maybe Text
guessForgeTypeFromURL urlString = do
  uri <- parseURI (toS urlString)
  autho <- uriAuthority uri
  let host = uriRegName autho
  if "github" `isInfixOf` host
    then Just "github"
    else
      if "gitlab" `isInfixOf` host
        then Just "gitlab"
        else Nothing
