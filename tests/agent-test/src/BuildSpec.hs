{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}

module BuildSpec where

import Control.Concurrent.STM.TVar (readTVar)
import Data.Aeson qualified as A
import Data.Map qualified as M
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUID
import Hercules.API.Agent.Build.BuildEvent qualified as BuildEvent
import Hercules.API.Agent.Build.BuildEvent.OutputInfo qualified as OutputInfo
import Hercules.API.Agent.Build.BuildTask qualified as BuildTask
import Hercules.API.Agent.Evaluate.EvaluateEvent
  ( EvaluateEvent,
  )
import Hercules.API.Agent.Evaluate.EvaluateEvent qualified as EvaluateEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEvent qualified as AttributeEvent
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
isAttrLike EvaluateEvent.Message {} = True -- this is a bit of a stretch but hey
isAttrLike _ = False

(=:) :: k -> a -> Map k a
(=:) = M.singleton

spec :: SpecWith ServerHandle
spec = describe "Build" do
  it "works" $ \srv -> do
    -- Setup: put the drv in the agent's store
    id <- randomId
    (s, r) <-
      runEval
        srv
        ( fixupInputs
            defaultEvalTask
              { EvaluateTask.id = id,
                EvaluateTask.otherInputs = "src" =: "/tarball/buildable" <> M.singleton "n" "/tarball/nixpkgs",
                EvaluateTask.autoArguments =
                  M.singleton
                    "nixpkgs"
                    (EvaluateTask.SubPathOf "n" Nothing),
                EvaluateTask.inputMetadata = "src" =: defaultMeta,
                EvaluateTask.selector = EvaluateTask.OnPush $ EvaluateTask.OnPush.MkOnPush {name = "ci", inputs = "nixpkgs" =: ImmutableInput.ArchiveUrl (apiBaseUrl <> "/tarball/nixpkgs")}
              }
        )
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
    -- runBuild _ (BuildTask.BuildTask {})
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

  it "prints cyclic output reference error" $ \srv -> do
    -- Setup: put the drv in the agent's store
    id <- randomId
    (s, r) <-
      runEval
        srv
        ( fixupInputs
            defaultEvalTask
              { EvaluateTask.id = id,
                EvaluateTask.otherInputs = "src" =: "/tarball/build-cyclic-output-reference" <> M.singleton "n" "/tarball/nixpkgs",
                EvaluateTask.autoArguments =
                  M.singleton
                    "nixpkgs"
                    (EvaluateTask.SubPathOf "n" Nothing),
                EvaluateTask.inputMetadata = "src" =: defaultMeta,
                EvaluateTask.selector = EvaluateTask.OnPush $ EvaluateTask.OnPush.MkOnPush {name = "ci", inputs = "nixpkgs" =: ImmutableInput.ArchiveUrl (apiBaseUrl <> "/tarball/nixpkgs")}
              }
        )
    s `shouldBe` TaskStatus.Successful ()
    drvPath <-
      case attrLike r of
        [EvaluateEvent.Attribute ae] -> do
          let drvPath = AttributeEvent.derivationPath ae
          AttributeEvent.expressionPath ae `shouldBe` ["it"]
          toS drvPath `shouldContain` "/nix/store"
          toS drvPath `shouldContain` "cyclic"
          pure drvPath
        _ -> failWith $ "Events should be a single attribute, not: " <> show r

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
    s2 `shouldBe` TaskStatus.Terminated ()
    case be of
      [] -> pass
      _ -> failWith $ "Didn't expect build events: " <> show be
    x <- timeout 30_000_000 do
      atomically do
        entries <- readTVar (logEntries $ serverState srv)
        -- unsafePerformIO do
        --   for_ (reverse entries) print
        --   pure pass

        guard $
          isJust $
            entries & find \case
              LogEntry.Msg {level = 0, msg = m}
                | "cycle detected"
                    `T.isInfixOf` m
                    && "outputOne"
                    `T.isInfixOf` m
                    && "outputTwo"
                    `T.isInfixOf` m ->
                    True
              _ -> False

      pass
    case x of
      Nothing -> failWith "Did not receive log in time."
      Just _ -> pass
