{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}

module EvaluationSpec where

import Data.Aeson qualified as A
import Data.List (last)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUID
import Hercules.API.Agent.Evaluate.EvaluateEvent
  ( EvaluateEvent,
  )
import Hercules.API.Agent.Evaluate.EvaluateEvent qualified as EvaluateEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEffectEvent qualified as AttributeEffectEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeErrorEvent qualified as AttributeErrorEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEvent qualified as AttributeEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequired qualified as BuildRequired
import Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo qualified as DerivationInfo
import Hercules.API.Agent.Evaluate.EvaluateEvent.Message qualified as Message
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnPushHandlerEvent qualified as OnPushHandlerEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent.OnScheduleHandlerEvent qualified as OnScheduleHandlerEvent
import Hercules.API.Agent.Evaluate.EvaluateTask (Identifier, Selector (ConfigOrLegacy))
import Hercules.API.Agent.Evaluate.EvaluateTask qualified as EvaluateTask
import Hercules.API.Agent.Evaluate.EvaluateTask.OnPush qualified as EvaluateTask.OnPush
import Hercules.API.Agent.Evaluate.ImmutableInput (ImmutableInput (ArchiveUrl))
import Hercules.API.Agent.Evaluate.ImmutableInput qualified as API.ImmutableInput
import Hercules.API.DayOfWeek (DayOfWeek (..))
import Hercules.API.Id (Id (Id))
import Hercules.API.Logs.LogEntry qualified as LogEntry
import Hercules.API.TaskStatus qualified as TaskStatus
import MockTasksApi
import Protolude
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

attrLike :: [EvaluateEvent] -> [EvaluateEvent]
attrLike = filter isAttrLike

isAttrLike :: EvaluateEvent -> Bool
isAttrLike EvaluateEvent.Attribute {} = True
isAttrLike EvaluateEvent.AttributeEffect {} = True
isAttrLike EvaluateEvent.AttributeError {} = True
isAttrLike EvaluateEvent.Message {} = True -- this is a bit of a stretch but hey
isAttrLike _ = False

(=:) :: k -> a -> Map k a
(=:) = M.singleton

defaultTask :: EvaluateTask.EvaluateTask
defaultTask =
  EvaluateTask.EvaluateTask
    { id = Prelude.error "override EvaluateTask.id please",
      primaryInput = "",
      inputMetadata = mempty,
      inputs = mempty,
      otherInputs = mempty,
      autoArguments = mempty,
      nixPath = mempty,
      logToken = "ignore-logs",
      selector = ConfigOrLegacy,
      ciSystems = Nothing,
      extraGitCredentials = Nothing,
      isFlakeJob = False
    }

srcInput :: Text -> Map Identifier ImmutableInput
srcInput url = M.singleton "src" (ArchiveUrl url)

defaultMeta :: Map Text A.Value
defaultMeta =
  "rev"
    =: A.String "eefe2e4df3a0f147cf0f59438010b63fd857291b"
    <> "ref"
    =: "refs/heads/main"

spec :: SpecWith ServerHandle
spec = describe "Evaluation" $ do
  context "when one of the attributes causes a stack overflow" $
    it "returns the message as an error alongside the successful derivations" $
      \srv -> do
        pendingWith "flaky test" -- "Did not receive log in time.", see below
        id <- randomId
        (s, _evalEvents) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/stack-overflow",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta,
                    EvaluateTask.logToken = "stack-overflow-log-token"
                  }
            )
        s `shouldBe` TaskStatus.Exceptional "Worker failed with exit status: 1"
        entries <- getLogs srv "stack-overflow-log-token"
        void $
          shouldBeJust $
            entries & find
              \case
                LogEntry.Msg {msg = msg} | "stack overflow" `T.isInfixOf` msg -> True
                _mismatched -> False

  context "when the source tarball cannot be fetched" $
    it "crashes with an error message" $
      \srv -> do
        pendingWith "slow test"
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    -- localhost because it (almost) always resolves and refuses
                    -- to connect quickly and :61 because it's a port number that
                    -- went from assigned to reserved in 2017 and is therefore
                    -- very very rarely used.
                    EvaluateTask.otherInputs = "src" =: "http://localhost:61/problem.tar.gz",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        let msg = case s of
              TaskStatus.Exceptional m -> m
              _ -> panic "task status"
        toS msg `shouldContain` "HttpExceptionRequest"
        toS msg `shouldContain` "Connection refused"
        r `shouldBe` []
  context "when the source download is not a valid tarball" $
    it "crashes with an error message" $
      \srv -> do
        pendingWith "slow test"
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/broken-tarball",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s
          `shouldBe` TaskStatus.Exceptional
            "SubprocessFailure {message = \"Extracting tarball\"}"
        r `shouldBe` []
  context "when the source download doesn't have a nix expression" $
    it "yields an error message" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/no-nix-file",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        case r of
          [EvaluateEvent.Message msg] ->
            msg
              `shouldBe` Message.Message
                { index = 0,
                  typ = Message.Error,
                  message = "Please provide a Nix expression to build. Could not find any of nix/ci.nix, ci.nix, flake.nix or default.nix in your source"
                }
          _ -> failWith $ "Events should be a single message, not: " <> show r
  context "when a ci.nix is provided" $
    it "it is preferred over default.nix and flake.nix" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/ci-dot-nix",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        case attrLike r of
          [EvaluateEvent.Attribute ae] -> do
            AttributeEvent.expressionPath ae `shouldBe` ["in-ci-dot-nix"]
            toS (AttributeEvent.derivationPath ae)
              `shouldContain` "-this-works.drv"
          _ ->
            failWith $ "Events should be a single attribute, not: " <> show r
  context "when both ci.nix and nix/ci.nix are provided" $
    it "an error is raised" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/ambiguous-nix-file",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        case r of
          [EvaluateEvent.Message msg] ->
            msg
              `shouldBe` Message.Message
                { index = 0,
                  typ = Message.Error,
                  message = "Don't know what to do, expecting only one of nix/ci.nix or ci.nix"
                }
          _ -> failWith $ "Events should be a single message, not: " <> show r
  context "when a ci.nix contains the herculesCI special attribute" do
    it "reports the onPush handler" \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          ( fixupInputs
              defaultTask
                { EvaluateTask.id = id,
                  EvaluateTask.otherInputs = "src" =: "/tarball/ci-dot-nix-herculesCI",
                  EvaluateTask.inputMetadata = "src" =: defaultMeta
                }
          )
      s `shouldBe` TaskStatus.Successful ()
      case r of
        [EvaluateEvent.JobConfig _jc, EvaluateEvent.OnPushHandlerEvent op] -> do
          OnPushHandlerEvent.handlerName op `shouldBe` "default"
          OnPushHandlerEvent.handlerExtraInputs op `shouldBe` mempty
        _ ->
          failWith $ "Events should be a [JobConfig, OnPushHandlerEvent op], not " <> show r
    it "reports the package" \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          ( fixupInputs
              defaultTask
                { EvaluateTask.id = id,
                  EvaluateTask.otherInputs = "src" =: "/tarball/ci-dot-nix-herculesCI",
                  EvaluateTask.inputMetadata = "src" =: defaultMeta,
                  EvaluateTask.selector =
                    EvaluateTask.OnPush $
                      EvaluateTask.OnPush.MkOnPush {name = "default", inputs = mempty}
                }
          )
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` ["packages", "x86_64-linux", "default"]
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae) `shouldContain` "-default-package"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r

  context "when a flake with onSchedule is provided" $
    it "reports the onSchedule handler" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/flake-onSchedule",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        case r of
          [EvaluateEvent.JobConfig _jc, EvaluateEvent.OnPushHandlerEvent _default, EvaluateEvent.OnScheduleHandlerEvent op] -> do
            OnScheduleHandlerEvent.handlerName op `shouldBe` "update"
            OnScheduleHandlerEvent.handlerExtraInputs op `shouldBe` mempty
            OnScheduleHandlerEvent.when op
              `shouldBe` OnScheduleHandlerEvent.TimeConstraints
                { minute = Just 37,
                  hour = Just [12],
                  dayOfWeek = Just [Mon, Tue, Wed, Thu, Fri, Sat, Sun],
                  dayOfMonth = Just [1, 2, 3, 4, 5, 6, 7]
                }
          _ ->
            failWith $ "Events should be a [JobConfig, EvaluateEvent.OnPushHandlerEvent, OnScheduleHandlerEvent], not " <> show r

  context "when a flake with onPush is provided" $
    it "reports the onPush handler" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/flake-onPush",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        case r of
          [EvaluateEvent.JobConfig _jc, EvaluateEvent.OnPushHandlerEvent op] -> do
            OnPushHandlerEvent.handlerName op `shouldBe` "default"
            OnPushHandlerEvent.handlerExtraInputs op `shouldBe` mempty
          _ ->
            failWith $ "Events should be a [JobConfig, OnPushHandlerEvent op], not " <> show r

  let simpleFlakeBehavior tarball = do
        it "reports the default onPush handler" do
          \srv -> do
            id <- randomId
            (s, r) <-
              runEval
                srv
                ( fixupInputs
                    defaultTask
                      { EvaluateTask.id = id,
                        EvaluateTask.otherInputs = "src" =: ("/tarball/" <> tarball),
                        EvaluateTask.inputMetadata = "src" =: defaultMeta
                      }
                )
            s `shouldBe` TaskStatus.Successful ()
            case r of
              [EvaluateEvent.JobConfig _jc, EvaluateEvent.OnPushHandlerEvent op] -> do
                OnPushHandlerEvent.handlerName op `shouldBe` "default"
                OnPushHandlerEvent.handlerExtraInputs op `shouldBe` mempty
              _ ->
                failWith $ "Events should be a [JobConfig, OnPushHandlerEvent op], not " <> show r
        it "reports the package" do
          \srv -> do
            id <- randomId
            (s, r) <-
              runEval
                srv
                ( fixupInputs
                    defaultTask
                      { EvaluateTask.id = id,
                        EvaluateTask.otherInputs = "src" =: ("/tarball/" <> tarball),
                        EvaluateTask.inputMetadata = "src" =: defaultMeta,
                        EvaluateTask.selector =
                          EvaluateTask.OnPush $
                            EvaluateTask.OnPush.MkOnPush {name = "default", inputs = mempty}
                      }
                )
            s `shouldBe` TaskStatus.Successful ()
            case attrLike r of
              [EvaluateEvent.Attribute ae] -> do
                AttributeEvent.expressionPath ae `shouldBe` ["packages", "x86_64-linux", "default"]
                toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
                toS (AttributeEvent.derivationPath ae) `shouldContain` "-default-package"
              _ -> failWith $ "Events should be a single attribute, not: " <> show r
  context "when a flake without onPush is provided" do
    simpleFlakeBehavior "flake"
  context "when a flake without onPush, with empty herculesCI is provided" do
    simpleFlakeBehavior "flake-herculesCI-empty"
  context "when a flake without onPush, with herculesCI.ciSystems is provided" do
    simpleFlakeBehavior "flake-ciSystems"
  context "when a flake with onPush, using functions is provided" do
    simpleFlakeBehavior "flake-onPush-functions"

  context "when the nix expression is one derivation in an attrset" $
    it "returns that attribute" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/simple",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        case attrLike r of
          [EvaluateEvent.Attribute ae] -> do
            AttributeEvent.expressionPath ae `shouldBe` ["hello"]
            toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
            toS (AttributeEvent.derivationPath ae)
              `shouldContain` "-myPackage.drv"
          _ -> failWith $ "Events should be a single attribute, not: " <> show r
  context "when specific attributes are set on the derivation attrset" $
    it "returns an attribute of the correct type" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/attribute-types",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        case attrLike r of
          [ EvaluateEvent.Attribute depsOnly,
            EvaluateEvent.AttributeEffect effect,
            EvaluateEvent.Attribute ignoreFail,
            EvaluateEvent.Attribute regular,
            EvaluateEvent.Attribute requireFail,
            EvaluateEvent.Attribute shell
            ] -> do
              AttributeEvent.expressionPath depsOnly `shouldBe` ["deps-only"]
              AttributeEffectEvent.expressionPath effect `shouldBe` ["effect"]
              AttributeEvent.expressionPath ignoreFail `shouldBe` ["ignore-fail"]
              AttributeEvent.expressionPath regular `shouldBe` ["regular"]
              AttributeEvent.expressionPath requireFail `shouldBe` ["require-fail"]
              AttributeEvent.expressionPath shell `shouldBe` ["shell"]
              AttributeEvent.typ depsOnly `shouldBe` AttributeEvent.DependenciesOnly
              AttributeEvent.typ ignoreFail `shouldBe` AttributeEvent.MayFail
              AttributeEvent.typ regular `shouldBe` AttributeEvent.Regular
              AttributeEvent.typ requireFail `shouldBe` AttributeEvent.MustFail
              AttributeEvent.typ shell `shouldBe` AttributeEvent.DependenciesOnly
          _ -> failWith $ "Attribute events should be six attributes, not: " <> show (attrLike r)
  context "when the nix expression is a naked derivation" $
    it "returns that attribute" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/naked-derivation",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        case attrLike r of
          [EvaluateEvent.Attribute ae] -> do
            AttributeEvent.expressionPath ae `shouldBe` []
            toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
            toS (AttributeEvent.derivationPath ae)
              `shouldContain` "-myPackage.drv"
          _ -> failWith $ "Events should be a single attribute, not: " <> show r
  context "when the nix expression is a list of derivations" $
    it "returns no events but succeed" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/list",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        case r of
          [EvaluateEvent.AttributeError AttributeErrorEvent.AttributeErrorEvent {errorMessage = msg}] -> do
            toS msg `shouldContain` "Expecting a value of type Attrs, but got type List."
          _ -> failWith $ "Events should be an single AttributeError, not " <> show r
  context "when the nix expression is an empty attrset" $
    it "returns no events but succeed" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/empty-attrset",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        case r of
          [] -> pass
          _ -> failWith $ "Events should be empty, not: " <> show r
  context
    "when the nix expression is a naked derivation behind default arguments"
    $ it "returns that attribute"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          ( fixupInputs
              defaultTask
                { EvaluateTask.id = id,
                  EvaluateTask.otherInputs = "src" =: "/tarball/naked-derivation-default-args",
                  EvaluateTask.inputMetadata = "src" =: defaultMeta
                }
          )
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` []
          toS (AttributeEvent.derivationPath ae)
            `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae)
            `shouldContain` "-myPackage.drv"
        _ ->
          failWith $ "Events should be a single attribute, not: " <> show r
  context "when the nix expression is an attribute set with further functions" $
    it "ignores the functions and return the derivations" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs =
                      "src"
                        =: "/tarball/naked-derivation-default-args-twice",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        case attrLike r of
          [EvaluateEvent.Attribute ae] -> do
            AttributeEvent.expressionPath ae `shouldBe` ["bar"]
            toS (AttributeEvent.derivationPath ae)
              `shouldContain` "/nix/store"
            toS (AttributeEvent.derivationPath ae)
              `shouldContain` "-myPackage.drv"
          _ ->
            failWith $ "Events should be a single attribute, not: " <> show r
  context "when the nix expression is an abort expression" $
    it "returns the message as an error" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/abort-at-root",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        case r of
          [EvaluateEvent.AttributeError ae] -> do
            AttributeErrorEvent.expressionPath ae `shouldBe` []
            toS (AttributeErrorEvent.errorMessage ae)
              `shouldContain` "evaluation aborted with the following error message"
            toS (AttributeErrorEvent.errorMessage ae)
              `shouldContain` "I refuse to do anything today."
          _ -> failWith $ "Events should be a single attribute, not: " <> show r
  context "when one of the attributes has an abort expression" $
    it "returns the message as an error alongside the successful derivations" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/abort-in-attribute",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        case attrLike r of
          [EvaluateEvent.AttributeError ae, EvaluateEvent.Attribute a] -> do
            AttributeEvent.expressionPath a `shouldBe` ["hello"]
            toS (AttributeEvent.derivationPath a) `shouldContain` "/nix/store"
            toS (AttributeEvent.derivationPath a)
              `shouldContain` "-myPackage.drv"
            AttributeErrorEvent.expressionPath ae
              `shouldBe` ["a-message-for-you"]
            toS (AttributeErrorEvent.errorMessage ae)
              `shouldContain` "evaluation aborted with the following error message"
            toS (AttributeErrorEvent.errorMessage ae)
              `shouldContain` "I am not doing this today."
          _ -> failWith $ "Wrong. It should not be: " <> show r
  context "when the source produces too many attributes" $
    it "yields an error message" $
      \srv -> do
        pendingWith "slow test"
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/too-many-attrs",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s
          `shouldBe` TaskStatus.Exceptional
            "Evaluation limit reached."
        last r
          `shouldBe` EvaluateEvent.Message
            ( Message.Message
                { index = 0,
                  typ = Message.Error,
                  message = "Evaluation limit reached. Does your nix expression produce infinite attributes? Please make sure that your project is finite. If it really does require more than 50000 attributes or messages, please contact info@hercules-ci.com."
                }
            )
  context "when the source produces too many messages" $
    it "yields an error message" $
      \srv -> do
        pendingWith
          "This does not produce the right kind of message, because they are still bound to attributes. It seems that we need builtins.trace reporting for this test case to be useful."
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/too-many-errors",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta
                  }
            )
        s
          `shouldBe` TaskStatus.Exceptional
            "WorkerException {originalException = FatalError {fatalErrorMessage = \"Evaluation limit reached.\"}, exitStatus = Nothing}"
        last r
          `shouldBe` EvaluateEvent.Message
            ( Message.Message
                { index = 0, -- This is the interesting part.
                  typ = Message.Error,
                  message = "Evaluation limit reached. Does your nix expression produce infinite attributes? Please make sure that your project is finite. If it really does require more than 50000 attributes or messages, please contact info@hercules-ci.com."
                }
            )
  context "when multiple messages are emitted" $
    it "assigns distinct index numbers to the messages" $
      \_env ->
        pendingWith "Need to emit multiple messages first"
  context "multiple inputs" $ do
    it "can add to NIX_PATH" $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          ( fixupInputs
              defaultTask
                { EvaluateTask.id = id,
                  EvaluateTask.otherInputs =
                    "src"
                      =: "/tarball/nix-path-simple"
                      <> "oi1"
                      =: "/tarball/simple",
                  EvaluateTask.nixPath =
                    [ EvaluateTask.NixPathElement
                        (Just "simple")
                        (EvaluateTask.SubPathOf "oi1" Nothing)
                    ],
                  EvaluateTask.inputMetadata = "src" =: defaultMeta
                }
          )
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` ["hello"]
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae)
            `shouldContain` "-myPackage.drv"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r
    it "can refer to nixpkgs" $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          ( fixupInputs
              defaultTask
                { EvaluateTask.id = id,
                  EvaluateTask.otherInputs = "src" =: "/tarball/nixpkgs-reference",
                  EvaluateTask.inputMetadata = "src" =: defaultMeta,
                  EvaluateTask.selector =
                    EvaluateTask.OnPush $
                      EvaluateTask.OnPush.MkOnPush
                        { name = "default",
                          inputs = "nixpkgs" =: API.ImmutableInput.ArchiveUrl (apiBaseUrl <> "/tarball/nixpkgs")
                        }
                }
          )
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` ["hello"]
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae) `shouldContain` "-hello"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r
  describe "when derivations are returned" $
    it "upload information about the closure under the inputDrv relation" $
      \srv -> do
        id <- randomId
        (s, r) <-
          runEval
            srv
            ( fixupInputs
                defaultTask
                  { EvaluateTask.id = id,
                    EvaluateTask.otherInputs = "src" =: "/tarball/nixpkgs-reference",
                    EvaluateTask.inputMetadata = "src" =: defaultMeta,
                    EvaluateTask.selector =
                      EvaluateTask.OnPush $
                        EvaluateTask.OnPush.MkOnPush
                          { name = "default",
                            inputs = "nixpkgs" =: API.ImmutableInput.ArchiveUrl (apiBaseUrl <> "/tarball/nixpkgs")
                          }
                  }
            )
        s `shouldBe` TaskStatus.Successful ()
        let drvMap ::
              Map
                DerivationInfo.DerivationPathText
                DerivationInfo.DerivationInfo
            drvMap = flip foldMap r $ \case
              EvaluateEvent.DerivationInfo drvInfo ->
                M.singleton (DerivationInfo.derivationPath drvInfo) drvInfo
              _ -> M.empty
        forM_ (toList drvMap) $ \drvInfo ->
          forM_ (M.keys $ DerivationInfo.inputDerivations drvInfo) $ \d ->
            unless (isJust (M.lookup d drvMap)) $
              panic $
                "In drv info for "
                  <> DerivationInfo.derivationPath drvInfo
                  <> ": unknown inputDrv "
                  <> d
        case attrLike r of
          [EvaluateEvent.Attribute ae] -> do
            AttributeEvent.expressionPath ae `shouldBe` ["hello"]
            let p = AttributeEvent.derivationPath ae
            toS p `shouldContain` "/nix/store"
            toS p `shouldContain` "-hello"
            isJust (M.lookup p drvMap) `shouldBe` True
          _ ->
            failWith $ "Events should be a single attribute, not: " <> show r
  describe "when builds are required for evaluation" $ do
    let ifdTest srv = do
          id <- randomId
          (s, r) <-
            runEval
              srv
              ( fixupInputs
                  defaultTask
                    { EvaluateTask.id = id,
                      EvaluateTask.otherInputs = "src" =: "/tarball/ifd",
                      EvaluateTask.selector =
                        EvaluateTask.OnPush $
                          EvaluateTask.OnPush.MkOnPush
                            { name = "default",
                              inputs = "nixpkgs" =: API.ImmutableInput.ArchiveUrl (apiBaseUrl <> "/tarball/nixpkgs")
                            },
                      EvaluateTask.inputMetadata = "src" =: defaultMeta
                    }
              )
          s `shouldBe` TaskStatus.Successful ()
          case attrLike r of
            [ EvaluateEvent.Attribute _nondetA,
              EvaluateEvent.Attribute _nondetB
              ] -> do
                pass
            bad -> do
              failWith $ "Events should have two attributes, not: " <> show bad
          do
            let attr = r & findJust "hello attribute" \case EvaluateEvent.Attribute a | AttributeEvent.expressionPath a == ["helloIfd"] -> Just a; _ -> Nothing
            void $ evaluate attr
            toS (AttributeEvent.derivationPath attr) `shouldContain` "/nix/store"
            toS (AttributeEvent.derivationPath attr) `shouldContain` "-hello"
          do
            let attr = r & findJust "figlet attribute" \case EvaluateEvent.Attribute a | AttributeEvent.expressionPath a == ["figletIfd"] -> Just a; _ -> Nothing
            toS (AttributeEvent.derivationPath attr) `shouldContain` "/nix/store"
            toS (AttributeEvent.derivationPath attr) `shouldContain` "-figlet"
          do
            let br1 = r & findJust "ifd-2 required" \case EvaluateEvent.BuildRequired a | "-ifd-2.nix.drv" `T.isSuffixOf` BuildRequired.derivationPath a -> Just a; _ -> Nothing
            toS (BuildRequired.derivationPath br1) `shouldContain` "/nix/store/"
            BuildRequired.outputName br1 `shouldBe` "out"
          do
            let br1 = r & findJust "ifd-2 required" \case EvaluateEvent.BuildRequired a | "-ifd-1.nix.drv" `T.isSuffixOf` BuildRequired.derivationPath a -> Just a; _ -> Nothing
            toS (BuildRequired.derivationPath br1) `shouldContain` "/nix/store/"
            BuildRequired.outputName br1 `shouldBe` "out"

    it "can request a build" ifdTest
    it "can request a build again" $ \srv -> ifdTest srv >> ifdTest srv
    it "can handle a failed build" $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          ( fixupInputs
              defaultTask
                { EvaluateTask.id = id,
                  EvaluateTask.otherInputs = "src" =: "/tarball/ifd-fail",
                  EvaluateTask.inputMetadata = "src" =: defaultMeta,
                  EvaluateTask.selector =
                    EvaluateTask.OnPush $
                      EvaluateTask.OnPush.MkOnPush
                        { name = "default",
                          inputs = "nixpkgs" =: API.ImmutableInput.ArchiveUrl (apiBaseUrl <> "/tarball/nixpkgs")
                        }
                }
          )
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [ _nondetA,
          _nondetB
          ] -> pass
        bad -> failWith $ "Events should have two attributes, not: " <> show bad

      do
        let attr = r & findJust "happy attribute" \case EvaluateEvent.Attribute a -> Just a; _ -> Nothing
        AttributeEvent.expressionPath attr `shouldBe` ["helloIfd"]
        toS (AttributeEvent.derivationPath attr) `shouldContain` "/nix/store"
        toS (AttributeEvent.derivationPath attr) `shouldContain` "-hello"
      do
        let attr = r & findJust "sad attribute" \case EvaluateEvent.AttributeError a -> Just a; _ -> Nothing
        AttributeErrorEvent.expressionPath attr `shouldBe` ["figletIfd"]
        length (AttributeErrorEvent.errorDerivation attr) `shouldBe` 1
        toS (fromMaybe "" (AttributeErrorEvent.errorDerivation attr)) `shouldContain` "/nix/store"
        toS (fromMaybe "" (AttributeErrorEvent.errorDerivation attr)) `shouldContain` "-ifd-2.nix"
        toS (AttributeErrorEvent.errorMessage attr) `shouldContain` "-ifd-2.nix"
        toS (AttributeErrorEvent.errorMessage attr) `shouldContain` "Could not build derivation"
        toS (AttributeErrorEvent.errorMessage attr) `shouldContain` "evaluat"
      do
        let br1 = r & findJust "ifd-2 required" \case EvaluateEvent.BuildRequired a | "-ifd-2.nix.drv" `T.isSuffixOf` BuildRequired.derivationPath a -> Just a; _ -> Nothing
        toS (BuildRequired.derivationPath br1) `shouldContain` "/nix/store/"
        BuildRequired.outputName br1 `shouldBe` "out"
      do
        let br1 = r & findJust "ifd-2 required" \case EvaluateEvent.BuildRequired a | "-ifd-1.nix.drv" `T.isSuffixOf` BuildRequired.derivationPath a -> Just a; _ -> Nothing
        toS (BuildRequired.derivationPath br1) `shouldContain` "/nix/store/"
        BuildRequired.outputName br1 `shouldBe` "out"

  describe "when using the herculesCI attribute based format" do
    it "rejects effects outside outputs.effects" $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          ( fixupInputs
              defaultTask
                { EvaluateTask.id = id,
                  EvaluateTask.otherInputs = "src" =: "/tarball/effect-attack",
                  EvaluateTask.selector =
                    EvaluateTask.OnPush $
                      EvaluateTask.OnPush.MkOnPush
                        { name = "default",
                          inputs = "nixpkgs" =: API.ImmutableInput.ArchiveUrl (apiBaseUrl <> "/tarball/nixpkgs")
                        },
                  EvaluateTask.inputMetadata = "src" =: defaultMeta
                }
          )
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [ EvaluateEvent.AttributeEffect a1,
          EvaluateEvent.AttributeError ae1
          ] -> do
            AttributeEffectEvent.expressionPath a1 `shouldBe` ["effects", "ok"]
            toS (AttributeEffectEvent.derivationPath a1) `shouldContain` "/nix/store"
            toS (AttributeEffectEvent.derivationPath a1) `shouldContain` "-effect-2"

            AttributeErrorEvent.expressionPath ae1 `shouldBe` ["illegal"]
            toS (AttributeErrorEvent.errorMessage ae1)
              `shouldContain` "only allowed below the effects attribute"
        bad -> failWith $ "Events should be a two attributes, not: " <> show bad
  describe "when the nix expression references a system path" $ do
    it "throws an error (1)" \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          ( fixupInputs
              defaultTask
                { EvaluateTask.id = id,
                  EvaluateTask.otherInputs = "src" =: "/tarball/path-attack",
                  EvaluateTask.inputMetadata = "src" =: defaultMeta
                }
          )
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [EvaluateEvent.AttributeError ae] -> do
          AttributeErrorEvent.expressionPath ae `shouldBe` ["hello"]
          fixup (AttributeErrorEvent.errorMessage ae) `shouldContain` "access to" -- "access to path", "access to absolute path"
          fixup (AttributeErrorEvent.errorMessage ae) `shouldContain` "/etc/hostname"
          fixup (AttributeErrorEvent.errorMessage ae) `shouldContain` "is forbidden in restricted mode"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r
    it "throws an error (2)" \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          ( fixupInputs
              defaultTask
                { EvaluateTask.id = id,
                  EvaluateTask.otherInputs = "src" =: "/tarball/path-attack-2",
                  EvaluateTask.inputMetadata = "src" =: defaultMeta
                }
          )
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [EvaluateEvent.AttributeError ae] -> do
          AttributeErrorEvent.expressionPath ae `shouldBe` ["hello"]
          fixup (AttributeErrorEvent.errorMessage ae) `shouldContain` "access to" -- "access to path", "access to absolute path"
          fixup (AttributeErrorEvent.errorMessage ae) `shouldContain` "/var/lib/hercules-ci-agent/secrets/secrets.json"
          fixup (AttributeErrorEvent.errorMessage ae) `shouldContain` "is forbidden in restricted mode"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r

fixup :: Text -> [Char]
fixup = noANSI . toS

-- Probably a bad implementation, but gets the job done for now and it's test code.
noANSI :: [Char] -> [Char]
noANSI ('\ESC' : cs) = noANSI . drop 1 . dropWhile (/= 'm') $ cs
noANSI (c : cs) = c : noANSI cs
noANSI [] = []

findJust :: Text -> (a -> Maybe b) -> [a] -> b
findJust err f l = l & mapMaybe f & headMay & fromMaybe (panic $ "Could not find " <> err)

shouldBeJust :: (HasCallStack) => Maybe a -> IO a
shouldBeJust = withFrozenCallStack \case
  Nothing -> do
    failWith "Expected Just, got Nothing"
  Just a -> pure a
