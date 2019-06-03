module Hercules.Agent.Cachix.Init where
import           Protolude

import qualified Cachix.Client.Push            as Cachix.Push
import qualified Cachix.Client.Secrets         as Cachix.Secrets
import qualified Cachix.Formats.CachixSigningKey
                                               as SigningKey
import qualified Cachix.Formats.CachixPullToken
                                               as PullToken
import qualified Cachix.Formats.CachixPublicKey
                                               as PublicKey
import           Hercules.Agent.Cachix.Env     as Env
import           Hercules.Agent.Log
import qualified Hercules.Agent.Config         as Config
import           Hercules.Error
import qualified Servant.Auth.Client

import qualified Katip                         as K
import qualified Katip.Core                    as K
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BC
import qualified Data.Aeson                    as Aeson
import           Data.Functor.Partitioner
import qualified Data.Map.Extras.Hercules      as ME
import qualified Data.Map                      as M
import qualified Data.List.NonEmpty            as NEL

readJSONLines :: FilePath -> IO [(Int, Aeson.Value)]
readJSONLines fname = do
  lines <-
    filter (not . BS.null . snd)
    . zip [1 ..]
    . BC.lines
    . toSL
    <$> BS.readFile fname
  escalate $ forM lines $ \(lineNo, res) -> case Aeson.eitherDecode (toS res) of
    Left e ->
      Left
        $ FatalError
        $ "JSON syntax error "
        <> toSL fname
        <> ":"
        <> show (lineNo :: Int)
        <> ": "
        <> toSL e
    Right r -> Right $ (lineNo, r)


mapLeft :: (e -> e') -> Either e Aeson.Value -> Either e' Aeson.Value
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right r) = Right (r :: Aeson.Value)

newEnv :: Config.Config -> K.LogEnv -> IO Env.Env
newEnv config logEnv = do
  jsons <-
    fmap fold $ forM (Config.cachixSecretsPath config) $ readJSONLines . toS

  let partitioner = (,,,) <$> part dec <*> part dec <*> part dec <*> part
        (\(ln, _json) -> Just (ln, "Unrecognized value"))
      dec :: Aeson.FromJSON a => (Int, Aeson.Value) -> Maybe a
      dec = toMaybe . Aeson.fromJSON . snd
       where
        toMaybe (Aeson.Success a) = Just a
        toMaybe _ = Nothing
      signingKeyList :: [SigningKey.CachixSigningKey]
      pullTokenList :: [PullToken.CachixPullToken]
      pubKeyList :: [PublicKey.CachixPublicKey]
      (signingKeyList, pullTokenList, pubKeyList, errors) =
        partitionList partitioner jsons

  forM_ errors $ \(ln, e) -> runKatipT logEnv $ K.logLoc
    (K.sl "filename" (Config.cachixSecretsPath config) <> K.sl "line" ln)
    mempty
    WarningS
    e

  let signingKeys = ME.groupOnNEL SigningKey.cacheName signingKeyList
      pullTokens = ME.groupOnNEL PullToken.cacheName pullTokenList

  pcs <- escalateAs FatalError $ forM signingKeys $ \sks ->
    let k = NEL.head sks
        name = SigningKey.cacheName k
        t = maybe "" (PullToken.secretToken . NEL.head) $ M.lookup name pullTokens
    in  do
          k' <- Cachix.Secrets.parseSigningKeyLenient $ SigningKey.secretKey k
          pure $ Cachix.Push.PushCache { pushCacheName = name
                                       , pushCacheSigningKey = k'
                                       , pushCacheToken = Servant.Auth.Client.Token $ toSL t
                                       }


  pure Env.Env
   { pushCaches = pcs
   , publicKeys = pubKeyList
   , netrcLines = toNetrcLines pullTokenList
   }

toNetrcLines :: [PullToken.CachixPullToken] -> [Text]
toNetrcLines = map toNetrcLine where
  toNetrcLine pt = "machine " <> PullToken.cacheName pt <> ".cachix.org"
                 <> " password " <> PullToken.secretToken pt
