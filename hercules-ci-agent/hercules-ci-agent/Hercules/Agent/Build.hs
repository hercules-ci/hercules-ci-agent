module Hercules.Agent.Build where

import           Protolude

import           Data.Char                      ( isSpace )
import           Data.Conduit.Process           ( sourceProcessWithStreams )
import qualified Data.Conduit.Combinators      as Conduit
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Hercules.Agent.Cachix         as Agent.Cachix
import qualified Hercules.Agent.Client
import           Hercules.Agent.Env
import           Hercules.Agent.Exception       ( defaultRetry )
import           Hercules.Agent.Log
import qualified Hercules.Agent.Nix            as Nix
import           Hercules.Agent.Nix.RetrieveDerivationInfo
                                                ( retrieveDerivationInfo )
import           Hercules.API                   ( noContent )
import           Hercules.API.Task              ( Task )
import qualified Hercules.API.Task             as Task
import qualified Hercules.API.Agent.Build.BuildEvent
                                               as BuildEvent
import qualified Hercules.API.Agent.Build.BuildEvent.OutputInfo
                                               as OutputInfo
import           Hercules.API.Agent.Build.BuildEvent.OutputInfo
                                                ( OutputInfo )
import qualified Hercules.API.Agent.Build.BuildEvent.Pushed
                                               as Pushed
import qualified Hercules.API.Agent.Build.BuildTask
                                               as BuildTask
import           Hercules.API.Agent.Build.BuildTask
                                                ( BuildTask )
import qualified Hercules.API.Agent.Build      as API.Build
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo
                                               as DerivationInfo
import qualified Hercules.API.Logs             as API.Logs
import           Servant.Auth.Client
import           System.Process

performBuild :: Task BuildTask.BuildTask -> App ()
performBuild task = do

  buildTask <- defaultRetry $ runHerculesClient
    (API.Build.getBuild Hercules.Agent.Client.buildClient (Task.id task))

  realise buildTask

  outs <- getOutputPathInfos buildTask

  reportOutputInfos buildTask outs

  push buildTask outs

  reportSuccess buildTask


realise :: BuildTask.BuildTask -> App ()
realise buildTask = do
  let stdinc = pass
      stdoutc = pass -- FIXME: use
      stderrc = Conduit.fold
  procSpec <-
    (\procSpec -> procSpec { close_fds = True -- Disable on Windows?
                           , cwd = Just "/"
    }) <$> Nix.nixProc
                   "nix-store"
                   [ "--realise"
                   , "--timeout"
                   , "36000" -- 10h TODO: make configurable via meta.timeout and decrease default to 3600s or so
                   , "--max-silent-time"
                   , "1800" -- 0.5h TODO: make configurable via (?) and decrease default to 600s
                   ]
                   [ BuildTask.derivationPath buildTask
                   ]

  logLocM DebugS $ "Invoking nix-store: " <> show procSpec

  (status, _out, errBytes) <- liftIO
    $ sourceProcessWithStreams procSpec stdinc stdoutc stderrc

  withNamedContext "exitStatus" (show status :: Text)
    $ logLocM DebugS
    $ "Returned from nix-store"

  noContent $ defaultRetry $ runHerculesClient' $ API.Logs.writeLog
    Hercules.Agent.Client.logsClient
    (Token $ toSL $ BuildTask.logToken buildTask)
    errBytes

  case status of
    ExitFailure e -> do
      withNamedContext "exitStatus" e $ logLocM ErrorS "Worker failed"

      emitEvents buildTask [BuildEvent.Done False]

      panic "Build failure" -- FIXME: this error is expected and should be handled normally
                            -- nonetheless it passes on to the tasks system

    ExitSuccess -> logLocM DebugS "Clean nix-store exit"


getOutputPathInfos :: BuildTask -> App (Map Text OutputInfo)
getOutputPathInfos buildTask = do
  let drvPath = BuildTask.derivationPath buildTask
  drvInfo <- retrieveDerivationInfo drvPath
  flip M.traverseWithKey (DerivationInfo.outputs drvInfo) $ \outputName o -> do
    let outputPath = DerivationInfo.path o

    sizeStr <- Nix.readNixProcess "nix-store" ["--query", "--size"] [toS outputPath] ""
    size <- case reads (toS $ T.dropAround isSpace $ toS sizeStr) of
      [(x, "")] -> pure x
      _ ->
        throwIO
          $ FatalError
          $ "nix-store returned invalid size "
          <> show sizeStr

    hashStr <- Nix.readNixProcess "nix-store" ["--query", "--hash"] [toS outputPath] ""
    let h = T.dropAround isSpace (toS hashStr)

    pure OutputInfo.OutputInfo { OutputInfo.deriver = drvPath
                               , name = outputName
                               , path = outputPath
                               , size = size
                               , hash = h
                               }


push :: BuildTask -> Map Text OutputInfo -> App ()
push buildTask outs = do
  let paths = OutputInfo.path <$> toList outs
  caches <- Agent.Cachix.activePushCaches
  forM_ caches $ \cache -> do
    Agent.Cachix.push cache paths
    emitEvents buildTask [BuildEvent.Pushed $ Pushed.Pushed { cache = cache }]

reportSuccess :: BuildTask -> App ()
reportSuccess buildTask = emitEvents buildTask [BuildEvent.Done True]


reportOutputInfos :: BuildTask -> Map Text OutputInfo -> App ()
reportOutputInfos buildTask outs =
  emitEvents buildTask $ map BuildEvent.OutputInfo (toList outs)


emitEvents :: BuildTask -> [BuildEvent.BuildEvent] -> App ()
emitEvents buildTask =
  noContent . defaultRetry . runHerculesClient . API.Build.updateBuild
    Hercules.Agent.Client.buildClient
    (BuildTask.id buildTask)
