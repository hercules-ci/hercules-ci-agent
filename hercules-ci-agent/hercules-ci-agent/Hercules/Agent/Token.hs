module Hercules.Agent.Token where

import           Protolude
import qualified Data.Text                     as T
import           Servant.Auth.Client            ( Token(Token) )

import qualified Hercules.API.Agents
import qualified Hercules.API.Agents.CreateAgent
                                               as CreateAgent
import           Hercules.Agent.Env            as Env
import           Hercules.Agent.Client          ( agentsClient )
import qualified System.Directory
import           System.FilePath                ( (</>) )

import           Network.BSD                    ( getHostName )
import           Hercules.Agent.CabalInfo      as CabalInfo
import           Hercules.Agent.Log

getDataDirectory :: MonadIO m => m FilePath
getDataDirectory = do
  liftIO $ System.Directory.getXdgDirectory System.Directory.XdgData
                                            "hercules-ci-agent"

writeAgentIdentity :: Text -> App ()
writeAgentIdentity tok = do
  dataDir <- getDataDirectory
  liftIO $ System.Directory.createDirectoryIfMissing True dataDir
  liftIO $ writeFile (dataDir </> "identity.token") (toS tok)

-- | Reads a token file, strips whitespace
readTokenFile :: MonadIO m => FilePath -> m Text
readTokenFile fp = do
  liftIO $ sanitize <$> readFile fp
 where
  sanitize = T.map subst . T.strip
  subst '\n' = ' '
  subst x = x

readAgentIdentity :: App (Maybe Text)
readAgentIdentity = do
  dataDir <- liftIO $ getDataDirectory
  logLocM DebugS $ "Data directory: " <> show dataDir
  let file = dataDir </> "identity.token"
  liftIO (System.Directory.doesFileExist file) >>= \case
    True -> notEmpty <$> readTokenFile file
     where
      notEmpty "" = Nothing
      notEmpty "x" = Nothing
      notEmpty token = Just token
    False -> pure Nothing

ensureAgentToken :: App Text
ensureAgentToken = do
  readAgentIdentity >>= \case
    Just x -> do
      logLocM DebugS "Found agent identity token"
      pure x
    Nothing -> do
      writeAgentIdentity "x" -- Sanity check
      logLocM DebugS "Creating agent identity token"
      agentSpecificToken <- createAgent
      logLocM DebugS "Agent identity token acquired"
      writeAgentIdentity agentSpecificToken
      logLocM DebugS "Agent identity token persisted"
      readAgentIdentity >>= \case
        Just x -> do
          logLocM DebugS "Found the new agent identity token"
          pure x
        Nothing ->
          panic
            "The file identity.token seems to have disappeared. Refusing to continue."

createAgent :: App Text
createAgent = do
  hostname <- liftIO getHostName
  let createAgentBody = CreateAgent.CreateAgent
        { hostname = toS hostname
        , agentVersion = CabalInfo.herculesAgentVersion -- TODO: Add git revision
        , nixVersion = "" -- FIXME
        , architectures = ["x86_64-linux"] -- FIXME
        }

  logLocM DebugS $ "CreateAgent data: " <> show createAgentBody
  token <- asks Env.agentToken
  agentIdentityToken <- runHerculesClient'
    $ Hercules.API.Agents.agentCreate agentsClient createAgentBody token
  pure agentIdentityToken

withAgentToken :: App a -> App a
withAgentToken m = do
  agentIdentityToken <- ensureAgentToken
  local (\env -> env { Env.agentToken = Token $ toS agentIdentityToken }) m
