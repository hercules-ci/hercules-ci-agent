module Hercules.Agent.Cachix.Init where
import           Protolude

import qualified Cachix.Client.Env             as Cachix.Env
import qualified Cachix.Client.OptionsParser   as Cachix.OptionsParser
import           Hercules.Agent.Cachix.Env     as Env
import           Hercules.Agent.Cachix.Info     as Info
import           Hercules.Agent.Log
import qualified Hercules.Agent.Config         as Config

import qualified System.Environment
import qualified Katip                         as K


newEnv :: Config.Config -> K.LogEnv -> IO Env.Env
newEnv config logEnv = do
  env' <- initCachix
  let env = Env.Env { cachixEnv = env', pushCaches = Config.cachixPushCaches config }
  runKatipT logEnv $ Info.validate `runReaderT` env
  pure env

initCachix :: IO Cachix.Env.Env
initCachix = do
  let bogusCommand = ["use", "&"]
  (opts, _cmd) <- System.Environment.withArgs bogusCommand
                                              Cachix.OptionsParser.getOpts
  Cachix.Env.mkEnv opts
