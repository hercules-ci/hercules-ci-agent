module Hercules.Agent.NixFile
  ( findNixFile,
  )
where

import Protolude
import qualified System.Directory as Dir
import System.FilePath ((</>))

type Ambiguity = [FilePath]

searchPath :: [Ambiguity]
searchPath = [["nix/ci.nix", "ci.nix"], ["default.nix"]]

findNixFile :: FilePath -> IO (Either Text FilePath)
findNixFile projectDir = do
  searchResult <-
    for searchPath $
      traverse $ \relPath ->
        let path = projectDir </> relPath
         in Dir.doesFileExist path >>= \case
              True -> pure $ Just (relPath, path)
              False -> pure Nothing
  case filter (not . null) $ map catMaybes searchResult of
    [(_relPath, unambiguous)] : _ -> pure (pure unambiguous)
    ambiguous : _ ->
      pure $
        Left $
          "Don't know what to do, expecting only one of "
            <> englishConjunction "or" (map fst ambiguous)
    [] ->
      pure $
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
