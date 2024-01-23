module Hercules.Agent.Token where

import Control.Lens ((^?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Base64.Lazy qualified as B64L
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Hercules.API.Agent.LifeCycle qualified
import Hercules.API.Agent.LifeCycle.CreateAgentSession_V2 qualified as CreateAgentSession
import Hercules.Agent.Client (lifeCycleClient)
import Hercules.Agent.Config (baseDirectory)
import Hercules.Agent.Env as Env
import Hercules.Agent.EnvironmentInfo qualified as EnvironmentInfo
import Hercules.Agent.Log
import Hercules.Error
import Protolude
import Servant.Auth.Client (Token (Token))
import System.Directory qualified
import System.FilePath ((</>))

getDir :: App FilePath
getDir = asks ((</> "secretState") . baseDirectory . config)

writeAgentSessionKey :: Text -> App ()
writeAgentSessionKey tok = do
  dir <- getDir
  liftIO $ System.Directory.createDirectoryIfMissing True dir
  liftIO $ writeFile (dir </> "session.key") (toS tok)

-- | Reads a token file, strips whitespace
readTokenFile :: (MonadIO m) => FilePath -> m Text
readTokenFile fp = liftIO $ sanitize <$> readFile fp
  where
    sanitize = T.map subst . T.strip
    subst '\n' = ' '
    subst x = x

readAgentSessionKey :: App (Maybe Text)
readAgentSessionKey = do
  dir <- getDir
  logLocM DebugS $ "Data directory: " <> logStr (show dir :: Text)
  let file = dir </> "session.key"
  liftIO (System.Directory.doesFileExist file) >>= \case
    True -> notEmpty <$> readTokenFile file
      where
        notEmpty "" = Nothing
        notEmpty "x" = Nothing
        notEmpty token = Just token
    False -> pure Nothing

ensureAgentSession :: App Text
ensureAgentSession =
  readAgentSessionKey >>= \case
    Just sessKey -> do
      logLocM DebugS "Found agent session key"
      let handler e = do
            logLocM WarningS $ "Failed to check whether session token matches cluster join token. Using old session. Remove session.key if you need to force a session update. Exception: " <> logStr (displayException e)
            pure sessKey
      safeLiftedHandle handler $ do
        cjt <- getClusterJoinTokenId
        logLocM DebugS $ "Found clusterJoinTokenId " <> logStr cjt
        scjt <- getSessionClusterJoinTokenId sessKey
        logLocM DebugS $ "Found sessionClusterJoinTokenId " <> logStr scjt
        if cjt == scjt
          then pure sessKey
          else do
            logLocM DebugS "Getting new session key to match cluster join token..."
            updateAgentSession
    Nothing -> do
      writeAgentSessionKey "x" -- Sanity check
      logLocM DebugS "Creating agent session"
      updateAgentSession

updateAgentSession :: App Text
updateAgentSession = do
  agentSessionKey <- createAgentSession
  logLocM DebugS "Agent session key acquired"
  writeAgentSessionKey agentSessionKey
  logLocM DebugS "Agent session key persisted"
  readAgentSessionKey >>= \case
    Just x -> do
      logLocM DebugS "Found the new agent session"
      pure x
    Nothing ->
      panic
        "The file session.key seems to have disappeared. Refusing to continue."

createAgentSession :: App Text
createAgentSession = do
  agentInfo <- EnvironmentInfo.extractAgentInfo
  logLocM DebugS $ "Agent info: " <> logStr (show agentInfo :: Text)
  let createAgentBody =
        CreateAgentSession.CreateAgentSession {agentInfo = agentInfo}
  token <- asks Env.currentToken
  runHerculesClient' $
    Hercules.API.Agent.LifeCycle.agentSessionCreate
      lifeCycleClient
      createAgentBody
      token

-- TODO: Although this looks nice, the implicit limitation here is that we can
--       only have one token at a time. I wouldn't be surprised if this becomes
--       problematic at some point. Perhaps we should switch to a polymorphic
--       reader monad like RIO when we hit that limitation.
withAgentToken :: App a -> App a
withAgentToken m = do
  agentSessionToken <- ensureAgentSession
  local (\env -> env {Env.currentToken = Token $ encodeUtf8 agentSessionToken}) m

data TokenError = TokenError Text
  deriving (Typeable, Show)

instance Exception TokenError

getClusterJoinTokenId :: App Text
getClusterJoinTokenId = do
  t <-
    asks Env.currentToken >>= \case
      Token jwt -> pure jwt
  v <- decodeToken (BL.fromStrict t)
  sub <- escalate $ maybeToEither (TokenError "No sub field in cluster join token") (v ^? key "sub" . _String)
  escalate $ maybeToEither (TokenError "Unrecognized token type") $ T.stripPrefix "t$" sub

getSessionClusterJoinTokenId :: Text -> App Text
getSessionClusterJoinTokenId t = do
  v <- decodeToken (BL.fromStrict $ encodeUtf8 t)
  escalate $ maybeToEither (TokenError "No parent field in session token") (v ^? key "parent" . _String)

decodeToken :: BL.ByteString -> App Aeson.Value
decodeToken bs = case BL.split (fromIntegral $ ord '.') bs of
  (_ : payload : _) -> escalateAs (TokenError . toS) $ Aeson.eitherDecode (B64L.decodeLenient payload)
  _ -> throwIO $ FatalError "JWT without periods?"
