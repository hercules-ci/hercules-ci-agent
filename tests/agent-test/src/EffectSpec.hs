{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}

module EffectSpec where

import Control.Concurrent.STM.TVar (readTVar)
import Data.Aeson qualified as A
import Data.Map qualified as M
import Data.UUID.V4 qualified as UUID
import Hercules.API.Agent.Effect.EffectTask qualified as EffectTask
import Hercules.API.Agent.Evaluate.EvaluateEvent (EvaluateEvent)
import Hercules.API.Agent.Evaluate.EvaluateEvent qualified as EvaluateEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEvent qualified as AttributeEffectEvent
import Hercules.API.Agent.Evaluate.EvaluateTask qualified as EvaluateTask
import Hercules.API.Agent.Evaluate.EvaluateTask.OnPush qualified as EvaluateTask.OnPush
import Hercules.API.Agent.Evaluate.ImmutableInput qualified as ImmutableInput
import Hercules.API.Id (Id (Id))
import Hercules.API.Logs.LogEntry qualified as LogEntry
import Hercules.API.TaskStatus qualified as TaskStatus
import MockTasksApi
import Protolude
import System.Timeout (timeout)
import Test.Hspec
import TestSupport (apiBaseUrl)
import Prelude
  ( error,
    userError,
  )
import Prelude qualified

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
      inputs = mempty,
      inputMetadata = mempty,
      nixPath = mempty,
      logToken = "mock-eval-log-token",
      selector = EvaluateTask.ConfigOrLegacy,
      ciSystems = Nothing,
      extraGitCredentials = mempty,
      isFlakeJob = False
    }

defaultMeta :: Map Text A.Value
defaultMeta =
  "rev"
    =: A.String "eefe2e4df3a0f147cf0f59438010b63fd857291b"
    <> "ref"
    =: "refs/heads/main"

attrLike :: [EvaluateEvent] -> [EvaluateEvent]
attrLike = filter isAttrLike

isAttrLike :: EvaluateEvent -> Bool
isAttrLike EvaluateEvent.Attribute {} = True
isAttrLike EvaluateEvent.AttributeError {} = True
isAttrLike EvaluateEvent.AttributeEffect {} = True
isAttrLike EvaluateEvent.Message {} = True -- this is a bit of a stretch but hey
isAttrLike _ = False

(=:) :: k -> a -> Map k a
(=:) = M.singleton

printErrItems :: (Foldable t, MonadIO f, Show a) => t a -> f ()
printErrItems items = for_ items \item -> putErrText ("  - " <> show item)

spec :: SpecWith ServerHandle
spec = describe "Effect" do
  it "fails effect when mountable is denied" $ \srv -> do
    -- Setup: put the drv in the agent's store
    id <- randomId
    (s, r) <-
      runEval
        srv
        ( fixupInputs
            defaultEvalTask
              { EvaluateTask.id = id,
                EvaluateTask.otherInputs = "src" =: "/tarball/effect" <> "n" =: "/tarball/nixpkgs",
                EvaluateTask.autoArguments =
                  M.singleton
                    "nixpkgs"
                    (EvaluateTask.SubPathOf "n" Nothing),
                EvaluateTask.inputMetadata = "src" =: defaultMeta,
                EvaluateTask.selector = EvaluateTask.OnPush $ EvaluateTask.OnPush.MkOnPush {name = "cd", inputs = "nixpkgs" =: ImmutableInput.ArchiveUrl (apiBaseUrl <> "/tarball/nixpkgs")}
              }
        )
    s `shouldBe` TaskStatus.Successful ()
    drvPath <-
      case attrLike r of
        [EvaluateEvent.AttributeEffect ev] -> do
          let drvPath = AttributeEffectEvent.derivationPath ev
          AttributeEffectEvent.expressionPath ev `shouldBe` ["effects", "launchIt"]
          toS drvPath `shouldContain` "/nix/store"
          pure drvPath
        attrEvs -> do
          putText "All events:"
          printErrItems r
          failWith $ "Events should be a single attribute, not: " <> show attrEvs
    -- Test: launch it
    id2 <- randomId
    s2 <-
      runEffect
        srv
        ( EffectTask.EffectTask
            { id = id2,
              derivationPath = drvPath,
              logToken = "pretend-jwt-for-log",
              inputDerivationOutputPaths = [],
              token = "test-fake-token",
              projectId = Id $ Prelude.read "00000000-0000-0000-0000-000000001234",
              projectPath = "github/test-fake-owner/test-fake-repo",
              serverSecrets = mempty,
              siteName = "test-fake-site",
              ownerName = "test-fake-owner",
              repoName = "test-fake-repo", -- ie not repo-with-shared-data
              ref = "refs/heads/test-fake-branch",
              isDefaultBranch = True
            }
        )
    case s2 of
      TaskStatus.Exceptional msg -> do
        let msg' = toS msg
        msg' `shouldContain` "/var/lib/shared-data"
        msg' `shouldContain` "a mountable with name shared-data has not been configured on agent, or it has been configured, but the condition field does not allow"
      _ -> failWith ("Unexpected status" <> show s2)

    -- TODO check that it didn't run. Need to add task id to log lines so that we don't get false results from other tests.
    pass

  it "works" $ \srv -> do
    -- Setup: put the drv in the agent's store
    id <- randomId
    (s, r) <-
      runEval
        srv
        ( fixupInputs
            defaultEvalTask
              { EvaluateTask.id = id,
                EvaluateTask.otherInputs = "src" =: "/tarball/effect" <> "n" =: "/tarball/nixpkgs",
                EvaluateTask.autoArguments =
                  M.singleton
                    "nixpkgs"
                    (EvaluateTask.SubPathOf "n" Nothing),
                EvaluateTask.inputMetadata = "src" =: defaultMeta,
                EvaluateTask.selector = EvaluateTask.OnPush $ EvaluateTask.OnPush.MkOnPush {name = "cd", inputs = "nixpkgs" =: ImmutableInput.ArchiveUrl (apiBaseUrl <> "/tarball/nixpkgs")}
              }
        )
    s `shouldBe` TaskStatus.Successful ()
    drvPath <-
      case attrLike r of
        [EvaluateEvent.AttributeEffect ev] -> do
          let drvPath = AttributeEffectEvent.derivationPath ev
          AttributeEffectEvent.expressionPath ev `shouldBe` ["effects", "launchIt"]
          toS drvPath `shouldContain` "/nix/store"
          pure drvPath
        attrEvs -> do
          putText "All events:"
          printErrItems r
          failWith $ "Events should be a single attribute, not: " <> show attrEvs
    -- Test: launch it
    id2 <- randomId
    s2 <-
      runEffect
        srv
        ( EffectTask.EffectTask
            { id = id2,
              derivationPath = drvPath,
              logToken = "pretend-jwt-for-log",
              inputDerivationOutputPaths = [],
              token = "test-fake-token",
              projectId = Id $ Prelude.read "00000000-0000-0000-0000-000000001234",
              projectPath = "github/test-fake-owner/test-fake-repo",
              serverSecrets = mempty,
              siteName = "test-fake-site",
              ownerName = "test-fake-owner",
              repoName = "repo-with-shared-data",
              ref = "refs/heads/test-fake-branch",
              isDefaultBranch = True
            }
        )
    s2 `shouldBe` TaskStatus.Successful ()
    x <- timeout 30_000_000 do
      atomically do
        entries <- readTVar (logEntries $ serverState srv)
        guard $
          isJust $
            entries & find \case LogEntry.Msg {msg = m} | m == "hello on stderr\r" -> True; _noMatch -> False
        guard $
          isJust $
            entries & find \case LogEntry.Msg {msg = m} | m == "hello on stdout\r" -> True; _noMatch -> False
        guard $
          isJust $
            entries & find \case LogEntry.Msg {msg = m} | m == "hi from src\r" -> True; _noMatch -> False
        guard $
          isJust $
            entries & find \case LogEntry.Msg {msg = m} | m == "hello from forwarded path\r" -> True; _noMatch -> False
        guard $
          isJust $
            entries & find \case LogEntry.Msg {msg = m} | m == "hello from shared-data\r" -> True; _noMatch -> False
        guard $
          isJust $
            entries & find \case LogEntry.Msg {msg = m} | m == "Hello, world! - The fake API testing endpoint\r" -> True; _noMatch -> False
      -- FIXME
      -- guard $
      --   isJust $
      --     entries & find \case LogEntry.Result { msg = m } | m == "log line without newline\r" -> True; _noMatch -> False
      pass
    case x of
      Nothing -> do
        printErrItems =<< atomically (readTVar (logEntries $ serverState srv))
        failWith "Did not receive satisfactory log in time."
      Just _ -> pass
