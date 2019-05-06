module BuildSpec where

import           Protolude
import           Prelude                        ( userError
                                                , error
                                                )

import qualified Data.Map                      as M
import qualified Data.UUID.V4                  as UUID
import           Hercules.API.Id                ( Id(Id) )
import qualified Hercules.API.Agent.Build.BuildTask
                                               as BuildTask
import qualified Hercules.API.Agent.Build.BuildEvent
                                               as BuildEvent
import qualified Hercules.API.Agent.Build.BuildEvent.OutputInfo
                                               as OutputInfo
import qualified Hercules.API.Agent.Evaluate.EvaluateTask
                                               as EvaluateTask
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent
                                               as EvaluateEvent
import           Hercules.API.Agent.Evaluate.EvaluateEvent
                                                ( EvaluateEvent )
import qualified Hercules.API.Agent.Evaluate.EvaluateEvent.AttributeEvent
                                               as AttributeEvent
import qualified Hercules.API.TaskStatus       as TaskStatus
import           MockTasksApi
import           Test.Hspec


randomId :: IO (Id a)
randomId = Id <$> UUID.nextRandom

failWith :: [Char] -> IO a
failWith = throwIO . userError

defaultEvalTask :: EvaluateTask.EvaluateTask
defaultEvalTask = EvaluateTask.EvaluateTask
  { id = Prelude.error "override EvaluateTask.id please"
  , primaryInput = Prelude.error "override EvaluateTask.primaryInput please"
  , otherInputs = mempty
  , autoArguments = mempty
  , nixPath = mempty
  }

attrLike :: [EvaluateEvent] -> [EvaluateEvent]
attrLike = filter isAttrLike

isAttrLike :: EvaluateEvent -> Bool
isAttrLike EvaluateEvent.Attribute{} = True
isAttrLike EvaluateEvent.AttributeError{} = True
isAttrLike EvaluateEvent.Message{} = True -- this is a bit of a stretch but hey
isAttrLike _ = False

spec :: SpecWith ServerHandle
spec = describe "Build" $ it "works" $ \srv -> do

      -- Setup: put the drv in the agent's store
  id <- randomId
  (s, r) <- runEval
    srv
    defaultEvalTask
      { EvaluateTask.id = id
      , EvaluateTask.primaryInput = "/tarball/buildable"
      , EvaluateTask.otherInputs = M.singleton "n" "/tarball/nixpkgs"
      , EvaluateTask.autoArguments = M.singleton
                                       "nixpkgs"
                                       (EvaluateTask.SubPathOf "n" Nothing)
      }

  s `shouldBe` TaskStatus.Successful ()

  drvPath <- case attrLike r of
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
  (s2, be) <- runBuild
    srv
    (BuildTask.BuildTask { id = id2
                         , derivationPath = drvPath
                         , logToken = "pretend-jwt-for-log"
                         }
    )
  s2 `shouldBe` TaskStatus.Successful ()

  case be of
    [BuildEvent.OutputInfo OutputInfo.OutputInfo { deriver = drvp, name = n, path = p, hash = h, size = sz }, BuildEvent.Done True]
      -> do
        toS drvp `shouldContain` "/nix/store"
        toS drvp `shouldContain` "one.drv"
        n `shouldBe` "out"
        toS p `shouldContain` "/nix/store"
        toS p `shouldContain` "-one"
        h
          `shouldBe` "sha256:15apcm9ksmd22hmxkmnncndgx1mx55nfan199rvbam8ygycr671b"
        sz `shouldBe` 120
    _ -> failWith $ "Didn't expect this: " <> show be
