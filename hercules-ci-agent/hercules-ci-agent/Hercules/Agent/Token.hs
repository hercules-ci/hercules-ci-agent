module Hercules.Agent.Token where

import           Protolude
import qualified Data.Text                     as T
import           Servant.Auth.Client            ( Token(Token) )

import qualified Hercules.API.Agents
import qualified Hercules.API.Agents.CreateAgentSession
                                               as CreateAgentSession
import           Hercules.Agent.Env            as Env
import           Hercules.Agent.Client          ( agentsClient )
import qualified System.Directory
import           System.FilePath                ( (</>) )

import           Network.BSD                    ( getHostName )
import           Hercules.Agent.CabalInfo      as CabalInfo
import           Hercules.Agent.Log

getDataDirectory :: MonadIO m => m FilePath
getDataDirectory =
  liftIO $ System.Directory.getXdgDirectory System.Directory.XdgData
                                            "hercules-ci-agent"

writeAgentSessionKey :: Text -> App ()
writeAgentSessionKey tok = do
  dataDir <- getDataDirectory
  liftIO $ System.Directory.createDirectoryIfMissing True dataDir
  liftIO $ writeFile (dataDir </> "session.key") (toS tok)

-- | Reads a token file, strips whitespace
readTokenFile :: MonadIO m => FilePath -> m Text
readTokenFile fp =
  liftIO $ sanitize <$> readFile fp
 where
  sanitize = T.map subst . T.strip
  subst '\n' = ' '
  subst x = x

readAgentSessionKey :: App (Maybe Text)
readAgentSessionKey = do
  dataDir <- liftIO $ getDataDirectory
  logLocM DebugS $ "Data directory: " <> show dataDir
  let file = dataDir </> "session.key"
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
  hostname <- liftIO getHostName
  let createAgentBody = CreateAgentSession.CreateAgentSession
        { hostname = toS hostname
        , agentVersion = CabalInfo.herculesAgentVersion -- TODO: Add git revision
        , nixVersion = "" -- FIXME
        , architectures = ["x86_64-linux"] -- FIXME
        }

  logLocM DebugS $ "CreateAgent data: " <> show createAgentBody
  token <- asks Env.currentToken
  runHerculesClient'
    $ Hercules.API.Agents.agentSessionCreate agentsClient createAgentBody token

-- TODO: Although this looks nice, the implicit limitation here is that we can
--       only have one token at a time. I wouldn't be surprised if this becomes
--       problematic at some point. Perhaps we should switch to a polymorphic
--       reader monad like RIO when we hit that limitation.
withAgentToken :: App a -> App a
withAgentToken m = do
  agentSessionToken <- ensureAgentSession
  local (\env -> env { Env.currentToken = Token $ toS agentSessionToken }) m
