module Hercules.Agent.Cachix.Init where
import           Protolude

import qualified Cachix.Client.Push            as Cachix.Push
import qualified Cachix.Client.Secrets         as Cachix.Secrets
import           Hercules.Agent.Cachix.Env     as Env
import qualified Hercules.Formats.CacheKeys    as CacheKeys
import qualified Hercules.Formats.CacheKeys.Keys
                                               as CacheKeys.Keys
import qualified Hercules.Agent.Config         as Config
import           Hercules.Error
import qualified Servant.Auth.Client

import qualified Katip                         as K
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Aeson                    as Aeson
import qualified Data.Map                      as M

readJSON :: Aeson.FromJSON a => FilePath -> IO a
readJSON fname = do
  bytes <- BL.readFile fname

  escalateAs
    (\e -> FatalError $ "JSON syntax error " <> toSL fname <> ":" <> toSL e)
    (Aeson.eitherDecode bytes)

mapLeft :: (e -> e') -> Either e Aeson.Value -> Either e' Aeson.Value
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right r) = Right (r :: Aeson.Value)

newEnv :: Config.Config -> K.LogEnv -> IO Env.Env
newEnv config _logEnv = do
  cks <-
    fmap (fromMaybe (CacheKeys.CacheKeys mempty))
    $ forM (Config.cachixSecretsPath config)
    $ readJSON . toS

  pcs <- toPushCaches cks

  pure Env.Env { pushCaches = pcs
               , netrcLines = toNetrcLines cks
               , cacheKeys = cks
               }

toNetrcLines :: CacheKeys.CacheKeys -> [Text]
toNetrcLines = concatMap toNetrcLine . M.toList . CacheKeys.caches where
  toNetrcLine (name, keys) = do
    pt <- toList $ CacheKeys.Keys.pullToken keys
    pure $ "machine " <> name <> ".cachix.org" <> " password " <> pt

toPushCaches :: CacheKeys.CacheKeys -> IO (Map Text Cachix.Push.PushCache)
toPushCaches = sequenceA . M.mapMaybeWithKey toPushCaches' . CacheKeys.caches where
  toPushCaches' name keys =
    let t = fromMaybe "" (CacheKeys.Keys.pullToken keys)
    in  do
          sk <- head $ CacheKeys.Keys.signingKeys keys
          Just $ escalateAs FatalError $ do
            k' <- Cachix.Secrets.parseSigningKeyLenient sk
            pure $ Cachix.Push.PushCache
              { pushCacheName = name
              , pushCacheSigningKey = k'
              , pushCacheToken = Servant.Auth.Client.Token $ toSL t
              }
