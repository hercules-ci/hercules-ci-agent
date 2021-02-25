module Hercules.CLI.Common where

import Hercules.CLI.Client
import Hercules.CLI.Credentials (determineDomain, readPersonalToken)
import Protolude
import RIO (RIO, runRIO)
import Servant.Auth.Client (Token (Token))

runAuthenticated :: RIO (HerculesClientToken, HerculesClientEnv) b -> IO b
runAuthenticated m = do
  clientEnv <- Hercules.CLI.Client.init
  domain <- determineDomain
  token <- readPersonalToken domain
  runRIO (HerculesClientToken $ Token $ encodeUtf8 token, clientEnv) m
