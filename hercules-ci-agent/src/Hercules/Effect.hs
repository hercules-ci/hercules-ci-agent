{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Effect where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Hercules.Agent.Sensitive (Sensitive (Sensitive, reveal), revealContainer)
import qualified Hercules.Formats.Secret as Formats.Secret
import Katip (KatipContext, Severity (..), logLocM, logStr)
import Protolude
import System.FilePath
import UnliftIO.Directory (createDirectoryIfMissing)

parseDrvSecretsMap :: Map ByteString ByteString -> Either Text (Map Text Text)
parseDrvSecretsMap drvEnv =
  case drvEnv & M.lookup "secretsMap" of
    Nothing -> pure mempty
    Just secretsMapText -> case A.eitherDecode (BL.fromStrict secretsMapText) of
      Left _ -> Left "Could not parse secretsMap variable in derivation. It must be a JSON dictionary of strings referencing agent secret names."
      Right r -> Right r

-- | Write secrets to file based on secretsMap value
writeSecrets :: (MonadIO m, KatipContext m) => FilePath -> Map Text Text -> Map Text (Sensitive Formats.Secret.Secret) -> FilePath -> m ()
writeSecrets sourceFile secretsMap extraSecrets destinationDirectory = write . fmap reveal . addExtra =<< gather
  where
    addExtra = flip M.union extraSecrets
    write = liftIO . BS.writeFile (destinationDirectory </> "secrets.json") . BL.toStrict . A.encode
    gather =
      if null secretsMap
        then pure mempty
        else do
          secretsBytes <- liftIO $ BS.readFile sourceFile
          r <- case A.eitherDecode $ BL.fromStrict secretsBytes of
            Left e -> do
              logLocM ErrorS $ "Could not parse secrets file " <> logStr sourceFile <> ": " <> logStr e
              throwIO $ FatalError "Could not parse secrets file as configured on agent."
            Right r -> pure (Sensitive r)
          createDirectoryIfMissing True destinationDirectory
          secretsMap & M.traverseWithKey \destinationName (secretName :: Text) -> do
            case revealContainer (r <&> M.lookup secretName) of
              Nothing ->
                liftIO $
                  throwIO $
                    FatalError $
                      "Secret " <> secretName <> " does not exist, so we can't find a secret for " <> destinationName <> ". Please make sure that the secret name matches a secret on your agents."
              Just ssecret ->
                pure do
                  secret <- ssecret
                  -- Currently this is `id` but we might want to fork the
                  -- format here or omit some fields.
                  pure $
                    Formats.Secret.Secret
                      { data_ = Formats.Secret.data_ secret
                      }
