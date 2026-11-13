{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}

-- | @hci curl@: run authenticated curl on the Hercules CI API.
module Hercules.CLI.Curl
  ( commandParser,
    resolveUrl,
    curlArguments,
  )
where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Hercules.CLI.Client (determineDefaultApiBaseUrl)
import Hercules.CLI.Credentials (determineDomain, readToken)
import Hercules.CLI.Exception (exitMsg)
import qualified Network.URI as URI
import qualified Options.Applicative as Optparse
import Protolude
import System.IO (hClose)
import System.IO.Error (isDoesNotExistError)
import System.Posix.IO (FdOption (CloseOnExec), createPipe, fdToHandle, setFdOption)
import System.Posix.Process (executeFile)
import System.Posix.Types (Fd)

commandParser :: Optparse.Parser (IO ())
commandParser = do
  url <-
    Optparse.strArgument
      ( Optparse.metavar "URL"
          <> Optparse.help "A path such as /api/v1/..., or an absolute URL on HERCULES_CI_API_BASE_URL's scheme, host and port"
      )
  curlArgs <-
    many $
      Optparse.strArgument
        ( Optparse.metavar "-- CURL_ARG..."
            <> Optparse.help "Arguments passed to curl as they are"
        )
  pure $ run url curlArgs

run :: Text -> [Text] -> IO ()
run url curlArgs = do
  baseUrl <- determineDefaultApiBaseUrl
  resolved <- either exitMsg pure (resolveUrl baseUrl url)
  token <- readToken determineDomain
  headerFd <- headerPipe ("Authorization: Bearer " <> token <> "\n")
  -- exec, so that curl's exit status, output and signals are the caller's own.
  executeFile "curl" True (map toS (curlArguments headerFd resolved curlArgs)) Nothing
    `catch` \e ->
      if isDoesNotExistError e
        then exitMsg "hci curl runs curl from PATH, but curl is not on PATH."
        else throwIO e

-- | A pipe holding the header, whose read end survives into curl.
--
-- The header goes to curl through a pipe, because argv is readable by other
-- processes.
headerPipe :: Text -> IO Fd
headerPipe header = do
  (readEnd, writeEnd) <- createPipe
  setFdOption readEnd CloseOnExec False
  setFdOption writeEnd CloseOnExec True
  h <- fdToHandle writeEnd
  -- A header is far smaller than a pipe's buffer, so this does not block.
  BS.hPut h (encodeUtf8 header)
  hClose h
  pure readEnd

-- | The arguments for curl. @--globoff@ keeps curl from expanding the URL
-- into others.
curlArguments :: Fd -> Text -> [Text] -> [Text]
curlArguments headerFd url curlArgs =
  ["--globoff", "--header", "@/dev/fd/" <> show headerFd, "--url", url] <> curlArgs

-- | Resolve the URL argument against the API base URL, refusing any URL the
-- token is not meant for.
--
-- A path goes after the base URL's path, and its query and fragment replace
-- the base URL's. An absolute URL must have the base URL's scheme, host and port, and no user
-- info. An omitted port is the scheme's default, so @https://host@ and
-- @https://host:443@ are the same.
resolveUrl :: Text -> Text -> Either Text Text
resolveUrl baseUrl url = do
  base <- parseUrl "HERCULES_CI_API_BASE_URL" baseUrl
  target <-
    if "/" `T.isPrefixOf` url
      then do
        ref <- maybeToEither ("URL is not a valid path: " <> url) (URI.parseRelativeReference (toS url))
        -- //host/path is a reference to another host, not a path.
        when (isJust (URI.uriAuthority ref)) $
          Left ("URL is not a path: " <> url)
        pure
          base
            { URI.uriPath = dropTrailingSlash (URI.uriPath base) <> URI.uriPath ref,
              URI.uriQuery = URI.uriQuery ref,
              URI.uriFragment = URI.uriFragment ref
            }
      else parseUrl "URL" url
  baseAuth <- maybeToEither "HERCULES_CI_API_BASE_URL has no host" (URI.uriAuthority base)
  targetAuth <- maybeToEither ("URL has no host: " <> url) (URI.uriAuthority target)
  let sameOrigin =
        T.toLower (toS (URI.uriScheme target)) == T.toLower (toS (URI.uriScheme base))
          && T.toLower (toS (URI.uriRegName targetAuth)) == T.toLower (toS (URI.uriRegName baseAuth))
          && port target targetAuth == port base baseAuth
          && URI.uriUserInfo targetAuth == ""
  unless sameOrigin $
    Left ("URL " <> url <> " is not on " <> baseUrl <> ", so it does not get the token")
  pure (toS (URI.uriToString identity target ""))
  where
    dropTrailingSlash = reverse . dropWhile (== '/') . reverse
    port uri auth = case (T.toLower (toS (URI.uriScheme uri)), URI.uriPort auth) of
      ("https:", "") -> ":443"
      ("http:", "") -> ":80"
      (_, p) -> toS p :: Text
    parseUrl what t =
      maybeToEither (what <> " is not a valid absolute URL: " <> t) (URI.parseAbsoluteURI (toS t))
