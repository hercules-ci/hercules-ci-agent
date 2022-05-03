module Hercules.CLI.Common
  ( runAuthenticated,
    runAuthenticatedOrDummy,
    exitMsg,
  )
where

import Hercules.CLI.Client
import Hercules.CLI.Credentials (determineDomain, readToken, tryReadEffectToken)
import Hercules.CLI.Exception (exitMsg)
import Protolude
import RIO (RIO, runRIO)
import Servant.Auth.Client (Token (Token))

runAuthenticated :: RIO (HerculesClientToken, HerculesClientEnv) b -> IO b
runAuthenticated m = do
  clientEnv <- Hercules.CLI.Client.init
  token <- readToken determineDomain
  runRIO (HerculesClientToken $ Token $ encodeUtf8 token, clientEnv) m
