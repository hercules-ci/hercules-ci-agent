{-# LANGUAGE BlockArguments #-}

module Hercules.CLI.Git where

import Data.List (dropWhileEnd)
import qualified Data.Text as T
import Hercules.CLI.Exception (exitMsg)
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
    x & toS
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
  pure $ sort $ ordNub (refs <> map ("refs/heads/" <>) (allBranches refs))

allBranches :: [Text] -> [Text]
allBranches = concatMap filterRef
  where
    filterRef ref =
      toList (T.stripPrefix "refs/heads/" ref)
        <|> T.dropWhile (== '/') . T.dropWhile (/= '/') <$> toList (T.stripPrefix "refs/remotes/" ref)

getAllBranches :: IO [Text]
getAllBranches = getRefs <&> allBranches

getUpstreamURL :: IO Text
getUpstreamURL = do
  remotes <- getRemotes
  case remotes of
    [x] -> getRemoteURL x
    _ -> do
      (getBranchUpstream >>= getRemoteURL) `onException` do
        putErrText "hci: could not determine git upstream repository url"

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

getRemoteURL :: Text -> IO Text
getRemoteURL remoteName =
  readProcessItem "git" ["remote", "get-url", toS remoteName] mempty
