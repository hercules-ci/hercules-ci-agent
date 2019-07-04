module Hercules.Agent.Token where

import           Protolude
import qualified Data.Text                     as T
import           Servant.Auth.Client            ( Token(Token) )

import qualified Hercules.API.Agent.LifeCycle
import qualified Hercules.API.Agents.CreateAgentSession_V2
                                               as CreateAgentSession
import           Hercules.Agent.Client          ( lifeCycleClient )
import           Hercules.Agent.Env            as Env
import           Hercules.Agent.Config          ( baseDirectory )
import qualified Hercules.Agent.EnvironmentInfo
                                               as EnvironmentInfo
import qualified System.Directory
import           System.FilePath                ( (</>) )

import           Hercules.Agent.Log

getDir :: App FilePath
getDir = asks ((</> "secretState") . baseDirectory . config)

writeAgentSessionKey :: Text -> App ()
writeAgentSessionKey tok = do
  dir <- getDir
  liftIO $ System.Directory.createDirectoryIfMissing True dir
  liftIO $ writeFile (dir </> "session.key") (toS tok)

-- | Reads a token file, strips whitespace
readTokenFile :: MonadIO m => FilePath -> m Text
readTokenFile fp = liftIO $ sanitize <$> readFile fp
 where
  sanitize = T.map subst . T.strip
  subst '\n' = ' '
  subst x = x

readAgentSessionKey :: App (Maybe Text)
readAgentSessionKey = do
  dir <- getDir
  logLocM DebugS $ "Data directory: " <> show dir
  let file = dir </> "session.key"
  liftIO (System.Directory.doesFileExist file) >>= \case
    True -> notEmpty <$> readTokenFile file
     where
      notEmpty "" = Nothing
      notEmpty "x" = Nothing
      notEmpty token = Just token
    False -> pure Nothing

ensureAgentSession :: App Text
ensureAgentSession = readAgentSessionKey >>= \case
  Just x -> do
    logLocM DebugS "Found agent session key"
    pure x
  Nothing -> do
    writeAgentSessionKey "x" -- Sanity check
    logLocM DebugS "Creating agent session"
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

  logLocM DebugS $ "Agent info: " <> show agentInfo

  let createAgentBody =
        CreateAgentSession.CreateAgentSession { agentInfo = agentInfo }
  token <- asks Env.currentToken
  runHerculesClient' $ Hercules.API.Agent.LifeCycle.agentSessionCreate
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
  local (\env -> env { Env.currentToken = Token $ toS agentSessionToken }) m
