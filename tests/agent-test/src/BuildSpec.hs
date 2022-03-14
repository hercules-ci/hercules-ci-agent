{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}

module BuildSpec where

import Control.Concurrent.STM.TVar (readTVar)
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.UUID.V4 as UUID
import qualified Hercules.API.Agent.Build.BuildEvent as BuildEvent
import qualified Hercules.API.Agent.Build.BuildEvent.OutputInfo as OutputInfo
import qualified Hercules.API.Agent.Build.BuildTask as BuildTask
import Hercules.API.Agent.Evaluate.EvaluateEvent
  ( EvaluateEvent,
  )
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent as EvaluateEvent
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEvent as AttributeEvent
import qualified Hercules.API.Agent.Evaluate.EvaluateTask as EvaluateTask
import qualified Hercules.API.Agent.Evaluate.ImmutableInput as ImmutableInput
import Hercules.API.Id (Id (Id))
import qualified Hercules.API.Logs.LogEntry as LogEntry
import qualified Hercules.API.TaskStatus as TaskStatus
import MockTasksApi
import Protolude
import System.Timeout (timeout)
import Test.Hspec
import TestSupport (apiBaseUrl)
import Prelude
  ( error,
    userError,
  )

randomId :: IO (Id a)
randomId = Id <$> UUID.nextRandom

failWith :: [Char] -> IO a
failWith = throwIO . userError

defaultEvalTask :: EvaluateTask.EvaluateTask
defaultEvalTask =
  EvaluateTask.EvaluateTask
    { id = Prelude.error "override EvaluateTask.id please",
      primaryInput = mempty,
      otherInputs = mempty,
      autoArguments = mempty,
      inputMetadata = mempty,
      nixPath = mempty,
      logToken = "mock-eval-log-token",
      selector = EvaluateTask.ConfigOrLegacy,
      extraGitCredentials = mempty
    }

defaultMeta :: Map Text A.Value
defaultMeta =
  "rev" =: A.String "eefe2e4df3a0f147cf0f59438010b63fd857291b"
    <> "ref" =: "refs/heads/main"

attrLike :: [EvaluateEvent] -> [EvaluateEvent]
attrLike = filter isAttrLike

isAttrLike :: EvaluateEvent -> Bool
isAttrLike EvaluateEvent.Attribute {} = True
isAttrLike EvaluateEvent.AttributeError {} = True
isAttrLike EvaluateEvent.Message {} = True -- this is a bit of a stretch but hey
isAttrLike _ = False

(=:) :: k -> a -> Map k a
(=:) = M.singleton

spec :: SpecWith ServerHandle
spec = describe "Build" $
  it "works" $ \srv -> do
    -- Setup: put the drv in the agent's store
    id <- randomId
    (s, r) <-
      runEval
        srv
        defaultEvalTask
          { EvaluateTask.id = id,
            EvaluateTask.otherInputs = "src" =: "/tarball/buildable" <> M.singleton "n" "/tarball/nixpkgs",
            EvaluateTask.autoArguments =
              M.singleton
                "nixpkgs"
                (EvaluateTask.SubPathOf "n" Nothing),
            EvaluateTask.inputMetadata = "src" =: defaultMeta,
            EvaluateTask.selector = EvaluateTask.OnPush $ EvaluateTask.MkOnPush {name = "ci", inputs = "nixpkgs" =: ImmutableInput.ArchiveUrl (apiBaseUrl <> "/tarball/nixpkgs")}
          }
    s `shouldBe` TaskStatus.Successful ()
    drvPath <-
      case attrLike r of
        [EvaluateEvent.Attribute ae] -> do
          let drvPath = AttributeEvent.derivationPath ae
          AttributeEvent.expressionPath ae `shouldBe` ["it"]
          toS drvPath `shouldContain` "/nix/store"
          toS drvPath `shouldContain` "-one"
          pure drvPath
        _ -> failWith $ "Events should be a single attribute, not: " <> show r
    -- Test: build it
    --runBuild _ (BuildTask.BuildTask {})
    id2 <- randomId
    (s2, be) <-
      runBuild
        srv
        ( BuildTask.BuildTask
            { id = id2,
              derivationPath = drvPath,
              logToken = "pretend-jwt-for-log",
              inputDerivationOutputPaths = []
            }
        )
    s2 `shouldBe` TaskStatus.Successful ()
    case be of
      [BuildEvent.OutputInfo OutputInfo.OutputInfo {deriver = drvp, name = n, path = p, hash = h, size = sz}, BuildEvent.Done True] ->
        do
          toS drvp `shouldContain` "/nix/store"
          toS drvp `shouldContain` "one.drv"
          n `shouldBe` "out"
          toS p `shouldContain` "/nix/store"
          toS p `shouldContain` "-one"
          h
            `shouldBe` "sha256:15apcm9ksmd22hmxkmnncndgx1mx55nfan199rvbam8ygycr671b"
          sz `shouldBe` 120
      _ -> failWith $ "Didn't expect this: " <> show be
    x <- timeout 30_000_000 do
      atomically do
        entries <- readTVar (logEntries $ serverState srv)
        guard $
          isJust $
            entries & find \case LogEntry.Result {rtype = LogEntry.ResultTypeBuildLogLine, fields = f} | toList f == [LogEntry.String "hello on stderr"] -> True; _ -> False
        guard $
          isJust $
            entries & find \case LogEntry.Result {rtype = LogEntry.ResultTypeBuildLogLine, fields = f} | toList f == [LogEntry.String "hello on stdout"] -> True; _ -> False
      pass
    case x of
      Nothing -> failWith "Did not receive log in time."
      Just _ -> pass
