{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Client.Commands
  ( authtoken,
    create,
    generateKeypair,
    push,
    use
    )
where

import qualified Cachix.Api as Api
import Cachix.Api.Error
import Cachix.Client.Config
  ( BinaryCacheConfig (..),
    Config (..),
    mkConfig,
    writeConfig
    )
import qualified Cachix.Client.Config as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.InstallationMode
import qualified Cachix.Client.NixConf as NixConf
import Cachix.Client.NixVersion (assertNixVersion)
import Cachix.Client.OptionsParser
  ( CachixOptions (..),
    PushArguments (..),
    PushOptions (..)
    )
import Cachix.Client.Push
import Cachix.Client.Secrets
  ( SigningKey (SigningKey),
    exportSigningKey,
    parseSigningKeyLenient
    )
import Cachix.Client.Servant
import qualified Cachix.Types.SigningKeyCreate as SigningKeyCreate
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (throwM)
import Control.Retry (RetryStatus (rsIterNumber))
import Crypto.Sign.Ed25519
import qualified Data.ByteString.Base64 as B64
import Data.List (isSuffixOf)
import Data.Maybe (fromJust)
import Data.String.Here
import qualified Data.Text as T
import Network.HTTP.Types (status401, status404)
import Protolude
import Servant.API (NoContent)
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FSNotify
import System.IO (hIsTerminalDevice, stdin)

authtoken :: Env -> Text -> IO ()
authtoken env token = do
  -- TODO: check that token actually authenticates!
  writeConfig (configPath (cachixoptions env)) $ case config env of
    Just config -> config {authToken = Token (toS token)}
    Nothing -> mkConfig token
  putStrLn
    ( [hereLit|
Continue by creating a binary cache at https://cachix.org
  |]
        :: Text
      )

create :: Env -> Text -> IO ()
create _ _ =
  throwIO $ DeprecatedCommand "Create command has been deprecated. Please visit https://cachix.org to create a binary cache."

generateKeypair :: Env -> Text -> IO ()
generateKeypair Env {config = Nothing} _ = throwIO $ NoConfig "Start with visiting https://cachix.org and copying the token to $ cachix authtoken <token>"
generateKeypair env@Env {config = Just config} name = do
  (PublicKey pk, sk) <- createKeypair
  let signingKey = exportSigningKey $ SigningKey sk
      signingKeyCreate = SigningKeyCreate.SigningKeyCreate (toS $ B64.encode pk)
      bcc = BinaryCacheConfig name signingKey
  -- we first validate if key can be added to the binary cache
  (_ :: NoContent) <-
    escalate
      =<< ( (`runClientM` clientenv env)
              $ Api.createKey (cachixBCClient name) (authToken config) signingKeyCreate
            )
  -- if key was successfully added, write it to the config
  -- TODO: this breaks if more than one key is added, see #27
  writeConfig (configPath (cachixoptions env))
    $ config {binaryCaches = binaryCaches config <> [bcc]}
  putStrLn
    ( [iTrim|
Secret signing key has been saved in the file above. To populate
your binary cache:

    $ nix-build | cachix push ${name}

Or if you'd like to use the signing key on another machine or CI:

    $ export CACHIX_SIGNING_KEY=${signingKey}
    $ nix-build | cachix push ${name}

To instruct Nix to use the binary cache:

    $ cachix use ${name}

IMPORTANT: Make sure to make a backup for the signing key above, as you have the only copy.
  |]
        :: Text
      )

envToToken :: Env -> Token
envToToken env =
  maybe (Token "") authToken (config env)

notAuthenticatedBinaryCache :: CachixException
notAuthenticatedBinaryCache =
  AccessDeniedBinaryCache "You must first authenticate using:  $ cachix authtoken <token>"

accessDeniedBinaryCache :: Text -> CachixException
accessDeniedBinaryCache name =
  AccessDeniedBinaryCache $ "You don't seem to have API access to binary cache " <> name

use :: Env -> Text -> UseOptions -> IO ()
use env name useOptions = do
  -- 1. get cache public key
  res <- (`runClientM` clientenv env) $ Api.get (cachixBCClient name) (envToToken env)
  case res of
    Left err
      | isErr err status401 && isJust (config env) -> throwM $ accessDeniedBinaryCache name
      | isErr err status401 -> throwM notAuthenticatedBinaryCache
      | isErr err status404 -> throwM $ BinaryCacheNotFound $ "Binary cache" <> name <> " does not exist."
      | otherwise -> throwM err
    Right binaryCache -> do
      () <- escalateAs UnsupportedNixVersion =<< assertNixVersion
      user <- getUser
      nc <- NixConf.read NixConf.Global
      isTrusted <- isTrustedUser $ NixConf.readLines (catMaybes [nc]) NixConf.isTrustedUsers
      isNixOS <- doesFileExist "/etc/NIXOS"
      let nixEnv = NixEnv
            { isRoot = user == "root",
              isTrusted = isTrusted,
              isNixOS = isNixOS
              }
      addBinaryCache (config env) binaryCache useOptions
        $ fromMaybe (getInstallationMode nixEnv) (useMode useOptions)

-- TODO: lots of room for perfomance improvements
push :: Env -> PushArguments -> IO ()
push env (PushPaths opts name rawPaths) = do
  hasNoStdin <- hIsTerminalDevice stdin
  when (not hasNoStdin && not (null rawPaths)) $ throwIO $ AmbiguousInput "You provided both stdin and store path arguments, pick only one to proceed."
  inputStorePaths <-
    if hasNoStdin
      then return rawPaths
      else T.words <$> getContents
  -- TODO: if empty, take whole nix store and warn: nix store-path --all
  when (null inputStorePaths) $ throwIO $ NoInput "You need to specify store paths either as stdin or as a cli argument"
  sk <- findSigningKey env name
  store <- wait (storeAsync env)
  void
    $ pushClosure
        (mapConcurrentlyBounded 4)
        (clientenv env)
        store PushCache
        { pushCacheToken = envToToken env,
          pushCacheName = name,
          pushCacheSigningKey = sk
          }
        (pushStrategy env opts name)
        inputStorePaths
  putText "All done."
push env (PushWatchStore opts name) = withManager $ \mgr -> do
  _ <- watchDir mgr "/nix/store" filterF action
  _ <- wait (storeAsync env)
  putText "Watching /nix/store for new builds ..."
  forever $ threadDelay 1000000
  where
    action :: Action
    action (Removed fp _ _) =
      pushStorePath env opts name $ toS $ dropEnd 5 fp
    action _ = return ()
    filterF :: ActionPredicate
    filterF (Removed fp _ _)
      | ".lock" `isSuffixOf` fp = True
    filterF _ = False
    dropEnd :: Int -> [a] -> [a]
    dropEnd index xs = take (length xs - index) xs

-- | Find a secret key in the 'Config' or environment variable
findSigningKey
  :: Env
  -> Text -- ^ Cache name
  -> IO SigningKey -- ^ Secret key or exception
findSigningKey env name = do
  maybeEnvSK <- lookupEnv "CACHIX_SIGNING_KEY"
  let matches Config {..} = filter (\bc -> Config.name bc == name) binaryCaches
      maybeBCSK = case config env of
        Nothing -> Nothing
        Just c -> Config.secretKey <$> head (matches c)
  -- TODO: better error msg
  escalateAs FatalError $ parseSigningKeyLenient $ fromJust $ maybeBCSK <|> toS <$> maybeEnvSK <|> panic "You need to: export CACHIX_SIGNING_KEY=XXX"

retryText :: RetryStatus -> Text
retryText retrystatus =
  if rsIterNumber retrystatus == 0
    then ""
    else "(retry #" <> show (rsIterNumber retrystatus) <> ") "

pushStrategy :: Env -> PushOptions -> Text -> Text -> PushStrategy IO ()
pushStrategy env opts name storePath = PushStrategy
  { onAlreadyPresent = pass,
    on401 =
      if isJust (config env)
        then throwM $ accessDeniedBinaryCache name
        else throwM notAuthenticatedBinaryCache,
    onError = throwM,
    onAttempt = \retrystatus ->
      -- we append newline instead of putStrLn due to https://github.com/haskell/text/issues/242
      putStr $ "pushing " <> retryText retrystatus <> storePath <> "\n",
    onDone = pass,
    withXzipCompressor = defaultWithXzipCompressorWithLevel (compressionLevel opts)
    }

pushStorePath :: Env -> PushOptions -> Text -> Text -> IO ()
pushStorePath env opts name storePath = do
  sk <- findSigningKey env name
  -- use secret key from config or env
  -- TODO: this shouldn't happen for each store path
  store <- wait (storeAsync env)
  pushSingleStorePath
    (clientenv env)
    store PushCache
    { pushCacheToken = envToToken env,
      pushCacheName = name,
      pushCacheSigningKey = sk
      }
    (pushStrategy env opts name storePath)
    storePath