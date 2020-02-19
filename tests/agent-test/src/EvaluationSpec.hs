module EvaluationSpec where

import Data.List (last)
import qualified Data.Map as M
import qualified Data.UUID.V4 as UUID
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent as EvaluateEvent
import Hercules.API.Agent.Evaluate.EvaluateEvent
  ( EvaluateEvent,
  )
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeErrorEvent as AttributeErrorEvent
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEvent as AttributeEvent
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.BuildRequired as BuildRequired
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.DerivationInfo as DerivationInfo
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.Message as Message
import qualified Hercules.API.Agent.Evaluate.EvaluateTask as EvaluateTask
import Hercules.API.Id (Id (Id))
import qualified Hercules.API.TaskStatus as TaskStatus
import MockTasksApi
import Protolude
import Test.Hspec
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
isAttrLike EvaluateEvent.AttributeError {} = True
isAttrLike EvaluateEvent.Message {} = True -- this is a bit of a stretch but hey
isAttrLike EvaluateEvent.BuildRequired {} = True -- this is a bit of a stretch but hey
isAttrLike _ = False

defaultTask :: EvaluateTask.EvaluateTask
defaultTask = EvaluateTask.EvaluateTask
  { id = Prelude.error "override EvaluateTask.id please",
    primaryInput = Prelude.error "override EvaluateTask.primaryInput please",
    otherInputs = mempty,
    autoArguments = mempty,
    nixPath = mempty
  }

spec :: SpecWith ServerHandle
spec = describe "Evaluation" $ do
  context "when the source tarball cannot be fetched"
    $ it "crashes with an error message"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              -- localhost because it (almost) always resolves and refuses
              -- to connect quickly and :61 because it's a port number that
              -- want from assigned to reserved in 2017 and is therefore
              -- very very rarely used.
              EvaluateTask.primaryInput = "http://localhost:61/problem.tar.gz"
            }
      let TaskStatus.Exceptional msg = s
      toS msg `shouldContain` "HttpExceptionRequest"
      toS msg `shouldContain` "Connection refused"
      r `shouldBe` []
  context "when the source download is not a valid tarball"
    $ it "crashes with an error message"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/broken-tarball"
            }
      s
        `shouldBe` TaskStatus.Exceptional
          "SubprocessFailure {message = \"Extracting tarball\"}"
      r `shouldBe` []
  context "when the source download doesn't have a nix expression"
    $ it "yields an error message"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/no-nix-file"
            }
      s `shouldBe` TaskStatus.Successful ()
      case r of
        [EvaluateEvent.Message msg] -> msg `shouldBe` Message.Message
          { index = 0,
            typ = Message.Error,
            message = "Please provide a Nix expression to build. Could not find any of \"nix/ci.nix\", \"ci.nix\" or \"default.nix\" in your source"
          }
        _ -> failWith $ "Events should be a single message, not: " <> show r
  context "when a ci.nix is provided"
    $ it "it is preferred over default.nix"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/ci-dot-nix"
            }
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` ["in-ci-dot-nix"]
          toS (AttributeEvent.derivationPath ae)
            `shouldContain` "-this-works.drv"
        _ ->
          failWith $ "Events should be a single attribute, not: " <> show r
  context "when both ci.nix and nix/ci.nix are provided"
    $ it "an error is raised"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/ambiguous-nix-file"
            }
      s `shouldBe` TaskStatus.Successful ()
      case r of
        [EvaluateEvent.Message msg] -> msg `shouldBe` Message.Message
          { index = 0,
            typ = Message.Error,
            message = "Don't know what to do, expecting only one of \"nix/ci.nix\" or \"ci.nix\""
          }
        _ -> failWith $ "Events should be a single message, not: " <> show r
  context "when the nix expression is one derivation in an attrset"
    $ it "returns that attribute"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/simple"
            }
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` ["hello"]
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae)
            `shouldContain` "-myPackage.drv"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r
  context "when the nix expression is a naked derivation"
    $ it "returns that attribute"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/naked-derivation"
            }
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` []
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae)
            `shouldContain` "-myPackage.drv"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r
  context "when the nix expression is a list of derivations"
    $ it "returns no events but succeed"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/list"
            }
      s `shouldBe` TaskStatus.Successful ()
      case r of
        [] -> pass
        _ -> failWith $ "Events should be empty, not: " <> show r
  context "when the nix expression is an empty attrset"
    $ it "returns no events but succeed"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/empty-attrset"
            }
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
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput =
                "/tarball/naked-derivation-default-args"
            }
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
  context "when the nix expression is an attribute set with further functions"
    $ it "ignores the functions and return the derivations"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput =
                "/tarball/naked-derivation-default-args-twice"
            }
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
  context "when the nix expression is an abort expression"
    $ it "returns the message as an error"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/abort-at-root"
            }
      s `shouldBe` TaskStatus.Successful ()
      case r of
        [EvaluateEvent.AttributeError ae] -> do
          AttributeErrorEvent.expressionPath ae `shouldBe` []
          toS (AttributeErrorEvent.errorMessage ae)
            `shouldContain` "evaluation aborted with the following error message: 'I refuse to do anything today.'"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r
  context "when one of the attributes has an abort expression"
    $ it "returns the message as an error alongside the successful derivations"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/abort-in-attribute"
            }
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
            `shouldContain` "evaluation aborted with the following error message: 'I am not doing this today.'"
        _ -> failWith $ "Wrong. It should not be: " <> show r
  context "when the source produces too many attributes"
    $ it "yields an error message"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/too-many-attrs"
            }
      s
        `shouldBe` TaskStatus.Exceptional
          "FatalError {fatalErrorMessage = \"Evaluation limit reached.\"}"
      last r
        `shouldBe` EvaluateEvent.Message
          ( Message.Message
              { index = 0,
                typ = Message.Error,
                message = "Evaluation limit reached. Does your nix expression produce infinite attributes? Please make sure that your project is finite. If it really does require more than 50000 attributes or messages, please contact info@hercules-ci.com."
              }
          )
  context "when the source produces too many messages"
    $ it "yields an error message"
    $ \srv -> do
      pendingWith
        "This does not produce the right kind of message, because they are still bound to attributes. It seems that we need builtins.trace reporting for this test case to be useful."
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/too-many-errors"
            }
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
  context "when multiple messages are emitted"
    $ it "assigns distinct index numbers to the messages"
    $ \_env ->
      pendingWith "Need to emit multiple messages first"
  context "multiple inputs" $ do
    it "can add to NIX_PATH" $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/nix-path-simple",
              EvaluateTask.otherInputs = M.singleton "oi1" "/tarball/simple",
              EvaluateTask.nixPath =
                [ EvaluateTask.NixPathElement
                    (Just "simple")
                    (EvaluateTask.SubPathOf "oi1" Nothing)
                ]
            }
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` ["hello"]
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae)
            `shouldContain` "-myPackage.drv"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r
    it "can add to auto arguments" $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/auto-arg-simple",
              EvaluateTask.otherInputs = M.singleton "oi1" "/tarball/simple",
              EvaluateTask.autoArguments =
                M.singleton "simple" (EvaluateTask.SubPathOf "oi1" Nothing)
            }
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
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/nixpkgs-reference",
              EvaluateTask.otherInputs = M.singleton "n" "/tarball/nixpkgs",
              EvaluateTask.autoArguments =
                M.singleton
                  "nixpkgs"
                  (EvaluateTask.SubPathOf "n" Nothing)
            }
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` ["hello"]
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae) `shouldContain` "-hello"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r
  describe "functor"
    $ it "is ok"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/functor",
              EvaluateTask.otherInputs = M.singleton "n" "/tarball/nixpkgs",
              EvaluateTask.autoArguments =
                M.singleton
                  "nixpkgs"
                  (EvaluateTask.SubPathOf "n" Nothing)
            }
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` ["foo", "a"]
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae) `shouldContain` "-zlib"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r
  describe "when derivations are returned"
    $ it "upload information about the closure under the inputDrv relation"
    $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/nixpkgs-reference",
              EvaluateTask.otherInputs = M.singleton "n" "/tarball/nixpkgs",
              EvaluateTask.autoArguments =
                M.singleton "nixpkgs" (EvaluateTask.SubPathOf "n" Nothing)
            }
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
          unless (isJust (M.lookup d drvMap))
            $ panic
            $ "In drv info for "
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
              defaultTask
                { EvaluateTask.id = id,
                  EvaluateTask.primaryInput = "/tarball/ifd",
                  EvaluateTask.otherInputs = M.singleton "n" "/tarball/nixpkgs",
                  EvaluateTask.autoArguments =
                    M.singleton
                      "nixpkgs"
                      (EvaluateTask.SubPathOf "n" Nothing)
                }
          s `shouldBe` TaskStatus.Successful ()
          case attrLike r of
            [ EvaluateEvent.BuildRequired br1,
              EvaluateEvent.Attribute ae1,
              EvaluateEvent.BuildRequired br2,
              EvaluateEvent.Attribute ae2
              ] -> do
                BuildRequired.index br1 `shouldBe` 0
                toSL (BuildRequired.derivationPath br1) `shouldContain` "/nix/store/"
                toSL (BuildRequired.derivationPath br1) `shouldContain` "-ifd-2.nix"
                BuildRequired.outputName br1 `shouldBe` "out"
                AttributeEvent.expressionPath ae1 `shouldBe` ["figletIfd"]
                toS (AttributeEvent.derivationPath ae1) `shouldContain` "/nix/store"
                toS (AttributeEvent.derivationPath ae1) `shouldContain` "-figlet"
                BuildRequired.index br2 `shouldBe` 1
                toSL (BuildRequired.derivationPath br2) `shouldContain` "/nix/store/"
                toSL (BuildRequired.derivationPath br2) `shouldContain` "-ifd-1.nix"
                BuildRequired.outputName br2 `shouldBe` "out"
                AttributeEvent.expressionPath ae2 `shouldBe` ["helloIfd"]
                toS (AttributeEvent.derivationPath ae2) `shouldContain` "/nix/store"
                toS (AttributeEvent.derivationPath ae2) `shouldContain` "-hello"
            bad -> failWith $ "Events should be a two attributes, not: " <> show bad
    it "can request a build" $ ifdTest
    it "can request a build again" $ \srv -> (ifdTest srv >> ifdTest srv)
    it "can handle a failed build" $ \srv -> do
      id <- randomId
      (s, r) <-
        runEval
          srv
          defaultTask
            { EvaluateTask.id = id,
              EvaluateTask.primaryInput = "/tarball/ifd-fail",
              EvaluateTask.otherInputs = M.singleton "n" "/tarball/nixpkgs",
              EvaluateTask.autoArguments =
                M.singleton
                  "nixpkgs"
                  (EvaluateTask.SubPathOf "n" Nothing)
            }
      s `shouldBe` TaskStatus.Successful ()
      case attrLike r of
        [ EvaluateEvent.BuildRequired br1,
          EvaluateEvent.AttributeError ae1,
          EvaluateEvent.BuildRequired br2,
          EvaluateEvent.Attribute ae2
          ] -> do
            BuildRequired.index br1 `shouldBe` 0
            toSL (BuildRequired.derivationPath br1) `shouldContain` "/nix/store/"
            toSL (BuildRequired.derivationPath br1) `shouldContain` "-ifd-2.nix"
            BuildRequired.outputName br1 `shouldBe` "out"
            AttributeErrorEvent.expressionPath ae1 `shouldBe` ["figletIfd"]
            length (AttributeErrorEvent.errorDerivation ae1) `shouldBe` 1
            toS (fromMaybe "" (AttributeErrorEvent.errorDerivation ae1)) `shouldContain` "/nix/store"
            toS (fromMaybe "" (AttributeErrorEvent.errorDerivation ae1)) `shouldContain` "-ifd-2.nix"
            toS (AttributeErrorEvent.errorMessage ae1) `shouldContain` "/nix/store"
            toS (AttributeErrorEvent.errorMessage ae1) `shouldContain` "-ifd-2.nix"
            toS (AttributeErrorEvent.errorMessage ae1) `shouldContain` "Could not build derivation"
            toS (AttributeErrorEvent.errorMessage ae1) `shouldContain` "evaluat"
            BuildRequired.index br2 `shouldBe` 1
            toSL (BuildRequired.derivationPath br2) `shouldContain` "/nix/store/"
            toSL (BuildRequired.derivationPath br2) `shouldContain` "-ifd-1.nix"
            BuildRequired.outputName br2 `shouldBe` "out"
            AttributeEvent.expressionPath ae2 `shouldBe` ["helloIfd"]
            toS (AttributeEvent.derivationPath ae2) `shouldContain` "/nix/store"
            toS (AttributeEvent.derivationPath ae2) `shouldContain` "-hello"
        bad -> failWith $ "Events should be a two attributes, not: " <> show bad
