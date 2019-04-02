module EvaluationSpec where

import           Protolude
import           Prelude                        ( userError
                                                , error
                                                )

import qualified Data.Map                      as M
import qualified Data.UUID.V4                  as UUID
import           Hercules.API.Id                ( Id(Id) )
import qualified Hercules.API.EvaluateTask     as EvaluateTask
import qualified Hercules.API.TaskStatus       as TaskStatus
import qualified Hercules.API.EvaluateEvent    as EvaluateEvent
import qualified Hercules.API.EvaluateEvent.AttributeEvent
                                               as AttributeEvent
import qualified Hercules.API.EvaluateEvent.AttributeErrorEvent
                                               as AttributeErrorEvent
import qualified Hercules.API.Message          as Message
import           MockTasksApi
import           Test.Hspec
import           Data.List                      ( last )

randomId :: IO (Id a)
randomId = Id <$> UUID.nextRandom

failWith :: [Char] -> IO a
failWith = throwIO . userError

defaultTask :: EvaluateTask.EvaluateTask
defaultTask = EvaluateTask.EvaluateTask
  { id = Prelude.error "override EvaluateTask.id please"
  , primaryInput = Prelude.error "override EvaluateTask.primaryInput please"
  , otherInputs = mempty
  , autoArguments = mempty
  , nixPath = mempty
  }

spec :: SpecWith ServerHandle
spec = describe "Evaluation" $ do
  context "when the source tarball cannot be fetched" $ do
    it "crashes with an error message" $ \srv -> do
      id <- randomId
      (s, r) <- runEval
        srv
        defaultTask
          { EvaluateTask.id = id

        -- localhost because it (almost) always resolves and refuses
        -- to connect quickly and :61 because it's a port number that
        -- want from assigned to reserved in 2017 and is therefore
        -- very very rarely used.
          , EvaluateTask.primaryInput = "http://localhost:61/problem.tar.gz"
          }
      let TaskStatus.Exceptional msg = s
      toS msg `shouldContain` "HttpExceptionRequest"
      toS msg `shouldContain` "Connection refused"
      r `shouldBe` []

  context "when the source download is not a valid tarball" $ do
    it "crashes with an error message" $ \srv -> do
      id <- randomId
      (s, r) <- runEval
        srv
        defaultTask { EvaluateTask.id = id
                    , EvaluateTask.primaryInput = "/tarball/broken-tarball"
                    }
      s `shouldBe` TaskStatus.Exceptional
        "SubprocessFailure {message = \"Extracting tarball\"}"
      r `shouldBe` []

  context "when the source download doesn't have a nix expression"
    $ it "yields an error message"
    $ \srv -> do
        id <- randomId
        (s, r) <- runEval
          srv
          defaultTask { EvaluateTask.id = id
                      , EvaluateTask.primaryInput = "/tarball/no-nix-file"
                      }
        s `shouldBe` TaskStatus.Successful ()
        case r of
          [EvaluateEvent.Message msg] -> msg `shouldBe` Message.Message
            { index = 0
            , typ = Message.Error
            , message =
              "Please provide a Nix expression to build. Could not find any of \"nix/ci.nix\", \"ci.nix\" or \"default.nix\" in your source"
            }

          _ -> failWith $ "Events should be a single message, not: " <> show r

  context "when a ci.nix is provided"
    $ it "it is preferred over default.nix"
    $ \srv -> do
        id <- randomId
        (s, r) <- runEval
          srv
          defaultTask { EvaluateTask.id = id
                      , EvaluateTask.primaryInput = "/tarball/ci-dot-nix"
                      }

        s `shouldBe` TaskStatus.Successful ()

        case r of
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
        (s, r) <- runEval
          srv
          defaultTask
            { EvaluateTask.id = id
            , EvaluateTask.primaryInput = "/tarball/ambiguous-nix-file"
            }
        s `shouldBe` TaskStatus.Successful ()
        case r of
          [EvaluateEvent.Message msg] -> msg `shouldBe` Message.Message
            { index = 0
            , typ = Message.Error
            , message = "Don't know what to do, expecting only one of \"nix/ci.nix\" or \"ci.nix\""
            }
          _ -> failWith $ "Events should be a single message, not: " <> show r

  context "when the nix expression is one derivation in an attrset" $ do
    it "returns that attribute" $ \srv -> do
      id <- randomId
      (s, r) <- runEval
        srv
        defaultTask { EvaluateTask.id = id
                    , EvaluateTask.primaryInput = "/tarball/simple"
                    }

      s `shouldBe` TaskStatus.Successful ()

      case r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` ["hello"]
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae)
            `shouldContain` "-myPackage.drv"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r

  context "when the nix expression is a naked derivation" $ do
    it "returns that attribute" $ \srv -> do
      id <- randomId
      (s, r) <- runEval
        srv
        defaultTask { EvaluateTask.id = id
                    , EvaluateTask.primaryInput = "/tarball/naked-derivation"
                    }

      s `shouldBe` TaskStatus.Successful ()

      case r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` []
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae)
            `shouldContain` "-myPackage.drv"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r

  context "when the nix expression is an empty attrset" $ do
    it "returns no events but succeed" $ \srv -> do
      id <- randomId
      (s, r) <- runEval
        srv
        defaultTask { EvaluateTask.id = id
                    , EvaluateTask.primaryInput = "/tarball/empty-attrset"
                    }

      s `shouldBe` TaskStatus.Successful ()

      case r of
        [] -> pass
        _ -> failWith $ "Events should be empty, not: " <> show r

  context
      "when the nix expression is a naked derivation behind default arguments"
    $ do
        it "returns that attribute" $ \srv -> do
          id <- randomId
          (s, r) <- runEval
            srv
            defaultTask
              { EvaluateTask.id = id
              , EvaluateTask.primaryInput =
                "/tarball/naked-derivation-default-args"
              }

          s `shouldBe` TaskStatus.Successful ()

          case r of
            [EvaluateEvent.Attribute ae] -> do
              AttributeEvent.expressionPath ae `shouldBe` []
              toS (AttributeEvent.derivationPath ae)
                `shouldContain` "/nix/store"
              toS (AttributeEvent.derivationPath ae)
                `shouldContain` "-myPackage.drv"
            _ ->
              failWith $ "Events should be a single attribute, not: " <> show r

  context "when the nix expression is an attribute set with further functions"
    $ do
        it "ignores the functions and return the derivations" $ \srv -> do
          id <- randomId
          (s, r) <- runEval
            srv
            defaultTask
              { EvaluateTask.id = id
              , EvaluateTask.primaryInput =
                "/tarball/naked-derivation-default-args-twice"
              }

          s `shouldBe` TaskStatus.Successful ()

          case r of
            [EvaluateEvent.Attribute ae] -> do
              AttributeEvent.expressionPath ae `shouldBe` ["bar"]
              toS (AttributeEvent.derivationPath ae)
                `shouldContain` "/nix/store"
              toS (AttributeEvent.derivationPath ae)
                `shouldContain` "-myPackage.drv"
            _ ->
              failWith $ "Events should be a single attribute, not: " <> show r

  context "when the nix expression is an abort expression" $ do
    it "returns the message as an error" $ \srv -> do
      id <- randomId
      (s, r) <- runEval
        srv
        defaultTask { EvaluateTask.id = id
                    , EvaluateTask.primaryInput = "/tarball/abort-at-root"
                    }

      s `shouldBe` TaskStatus.Successful ()

      case r of
        [EvaluateEvent.AttributeError ae] -> do
          AttributeErrorEvent.expressionPath ae `shouldBe` []
          toS (AttributeErrorEvent.errorMessage ae)
            `shouldContain` "evaluation aborted with the following error message: 'I refuse to do anything today.'"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r

  context "when one of the attributes has an abort expression" $ do
    it "returns the message as an error alongside the successful derivations"
      $ \srv -> do
          id <- randomId
          (s, r) <- runEval
            srv
            defaultTask
              { EvaluateTask.id = id
              , EvaluateTask.primaryInput = "/tarball/abort-in-attribute"
              }

          s `shouldBe` TaskStatus.Successful ()

          case r of
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

  context "when the source produces too many attributes" $ do
    it "yields an error message" $ \srv -> do
      id <- randomId
      (s, r) <- runEval
        srv
        defaultTask { EvaluateTask.id = id
                    , EvaluateTask.primaryInput = "/tarball/too-many-attrs"
                    }
      s `shouldBe` TaskStatus.Exceptional
        "FatalError {fatalErrorMessage = \"Evaluation limit reached.\"}"
      last r `shouldBe` EvaluateEvent.Message
        (Message.Message
          { index = 0
          , typ = Message.Error
          , message = "Evaluation limit reached. Does your nix expression produce infinite attributes? Please make sure that your project is finite. If it really does require more than 10000 attributes or messages, please contact info@hercules-ci.com."
          }
        )

  context "when the source produces too many messages" $ do
    it "yields an error message" $ \srv -> do
      pendingWith
        "This does not produce the right kind of message, because they are still bound to attributes. It seems that we need builtins.trace reporting for this test case to be useful."
      id <- randomId
      (s, r) <- runEval
        srv
        defaultTask { EvaluateTask.id = id
                    , EvaluateTask.primaryInput = "/tarball/too-many-errors"
                    }
      s `shouldBe` TaskStatus.Exceptional
        "FatalError {fatalErrorMessage = \"Evaluation limit reached.\"}"
      last r `shouldBe` EvaluateEvent.Message
        (Message.Message
          { index = 0 -- This is the interesting part.
          , typ = Message.Error
          , message = "Evaluation limit reached. Does your nix expression produce infinite attributes? Please make sure that your project is finite. If it really does require more than 10000 attributes or messages, please contact info@hercules-ci.com."
          }
        )

  context "when multiple messages are emitted" $ do
    it "assigns distinct index numbers to the messages" $ \_env -> do
      pendingWith "Need to emit multiple messages first"

  context "multiple inputs" $ do
    it "can add to NIX_PATH" $ \srv -> do
      id <- randomId
      (s, r) <- runEval
        srv
        defaultTask
          { EvaluateTask.id = id
          , EvaluateTask.primaryInput = "/tarball/nix-path-simple"
          , EvaluateTask.otherInputs = M.singleton "oi1" "/tarball/simple"
          , EvaluateTask.nixPath = [ EvaluateTask.NixPathElement
                                       (Just "simple")
                                       (EvaluateTask.SubPathOf "oi1" Nothing)
                                   ]
          }

      s `shouldBe` TaskStatus.Successful ()

      case r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` ["hello"]
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae)
            `shouldContain` "-myPackage.drv"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r

    it "can add to auto arguments" $ \srv -> do
      id <- randomId
      (s, r) <- runEval
        srv
        defaultTask
          { EvaluateTask.id = id
          , EvaluateTask.primaryInput = "/tarball/auto-arg-simple"
          , EvaluateTask.otherInputs = M.singleton "oi1" "/tarball/simple"
          , EvaluateTask.autoArguments =
            M.singleton "simple" (EvaluateTask.SubPathOf "oi1" Nothing)
          }

      s `shouldBe` TaskStatus.Successful ()

      case r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` ["hello"]
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae)
            `shouldContain` "-myPackage.drv"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r

    it "can refer to nixpkgs" $ \srv -> do
      id <- randomId
      (s, r) <- runEval
        srv
        defaultTask
          { EvaluateTask.id = id
          , EvaluateTask.primaryInput = "/tarball/nixpkgs-reference"
          , EvaluateTask.otherInputs = M.singleton "n" "/tarball/nixpkgs"
          , EvaluateTask.autoArguments = M.singleton
                                           "nixpkgs"
                                           (EvaluateTask.SubPathOf "n" Nothing)
          }

      s `shouldBe` TaskStatus.Successful ()

      case r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` ["hello"]
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae) `shouldContain` "-hello"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r

  describe "functor" $ do
    it "is ok" $ \srv -> do
      id <- randomId
      (s, r) <- runEval
        srv
        defaultTask
          { EvaluateTask.id = id
          , EvaluateTask.primaryInput = "/tarball/functor"
          , EvaluateTask.otherInputs = M.singleton "n" "/tarball/nixpkgs"
          , EvaluateTask.autoArguments = M.singleton
                                           "nixpkgs"
                                           (EvaluateTask.SubPathOf "n" Nothing)
          }

      s `shouldBe` TaskStatus.Successful ()

      case r of
        [EvaluateEvent.Attribute ae] -> do
          AttributeEvent.expressionPath ae `shouldBe` ["foo", "a"]
          toS (AttributeEvent.derivationPath ae) `shouldContain` "/nix/store"
          toS (AttributeEvent.derivationPath ae) `shouldContain` "-zlib"
        _ -> failWith $ "Events should be a single attribute, not: " <> show r
