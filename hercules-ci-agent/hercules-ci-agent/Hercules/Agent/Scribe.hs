{-# LANGUAGE ScopedTypeVariables #-}
module Hercules.Agent.Scribe
  ( withHerculesScribe
  )
where

import           Protolude

import           Data.Aeson                     ( Object
                                                , Value(Object)
                                                , object
                                                )
import           Hercules.Agent.Env             ( App
                                                , runApp
                                                , runHerculesClient
                                                )
import           Hercules.Agent.Client          ( tasksClient )
import           Hercules.Agent.Batch
import           Hercules.API
import           Hercules.API.Agent.Tasks
import           Katip

withHerculesScribe :: App a -> App a
withHerculesScribe m = withHerculesScribe' V3 $ \scribe -> do
  logEnv0 <- getLogEnv
  logEnv1 <- liftIO
    $ registerScribe "hercules" scribe defaultScribeSettings logEnv0
  localLogEnv (const logEnv1) $ m

-- TODO: batcher crash?
-- TODO: backpressure and/or message dropping?
withHerculesScribe' :: Verbosity -> (Scribe -> App a) -> App a
withHerculesScribe' verbosity m = do
  env <- ask
  let submitBatch values =
        noContent $ runApp env $ runHerculesClient (postLog tasksClient values)

  chan <- liftIO newChan


  let oneSecond = 1000000
      maxDelay = 3 * oneSecond
      maxItems = 100

      ensureObject :: Value -> Object
      ensureObject (Object o) = o
      ensureObject x = ensureObject (object [("value", x)]) -- Doesn't happen

  liftIO
    $ boundedDelayBatcher maxDelay maxItems chan submitBatch
    $ \batchResult ->
        let push :: LogItem a => Item a -> IO ()
            push li = writeChan chan $ Payload $ ensureObject $ itemJson
              verbosity
              li
            finalize :: IO ()
            finalize = do
              writeChan chan $ Stop ()
              wait batchResult
        in  runApp env $ m $ Scribe {liPush = push, scribeFinalizer = finalize}
