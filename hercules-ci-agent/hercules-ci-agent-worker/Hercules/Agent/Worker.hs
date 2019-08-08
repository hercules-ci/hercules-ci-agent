{-# LANGUAGE DataKinds #-}

module Hercules.Agent.Worker
  ( main
  )
where

import           Prelude                        ( )
import           Protolude               hiding ( evalState )

import           Conduit
import           Control.Concurrent.STM
import           CNix
import qualified CNix.Internal.Raw
import qualified Data.ByteString               as BS
import           Data.Conduit.Serialization.Binary
                                                ( conduitEncode
                                                , conduitDecode
                                                )
import qualified Data.Conduit
import           Data.Conduit.Extras            ( sinkChan, sourceChan )
import           Data.IORef
import           Data.List                      ( last )
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Hercules.Agent.WorkerProtocol.Command
                                               as Command
import qualified Hercules.Agent.WorkerProtocol.Command.Eval
                                               as Eval
import qualified Hercules.Agent.WorkerProtocol.Event
                                               as Event
import qualified Hercules.Agent.WorkerProtocol.Event.Attribute
                                               as Attribute
import qualified Hercules.Agent.WorkerProtocol.Event.AttributeError
                                               as AttributeError
import           Hercules.Agent.WorkerProtocol.Command
                                                ( Command )
import           Hercules.Agent.WorkerProtocol.Command.Eval
                                                ( Eval )
import qualified Hercules.Agent.WorkerProtocol.Command.BuildResult
                                               as BuildResult
import           Hercules.Agent.WorkerProtocol.Event
                                                ( Event )
import qualified Language.C.Inline.Cpp.Exceptions
                                               as C

data HerculesState = HerculesState
  { drvsCompleted :: TVar (Map Text BuildResult.BuildStatus)
  , drvsInProgress :: IORef (Set Text)
  , herculesStore :: Ptr (Ref HerculesStore)
  , wrappedStore :: Ptr (Ref NixStore)
  , shortcutChannel :: Chan (Maybe Event)
  }

main :: IO ()
main = do
  hPutStrLn stderr ("Initializing store..." :: Text)
  CNix.init

  drvsCompleted_ <- newTVarIO mempty
  drvsInProgress_ <- newIORef mempty

  withStore $ \wrappedStore_ -> withHerculesStore wrappedStore_ $ \herculesStore_ -> do
    setBuilderCallback herculesStore_ mempty

    ch <- liftIO newChan

    let st = HerculesState
              { drvsCompleted = drvsCompleted_
              , drvsInProgress = drvsInProgress_
              , herculesStore = herculesStore_
              , wrappedStore = wrappedStore_
              , shortcutChannel = ch
              }

    void $ concurrently
      (runConduitRes
        (sourceHandle stdin
        .| conduitDecode
        .| printCommands
        .| runCommands st
        .| sinkChan ch
        )
      )
      (runConduitRes
        (sourceChan ch
        .| conduitEncode
        .| concatMapC (\x -> [Chunk x, Flush])
        .| sinkHandleFlush stdout
        )
      )

printCommands :: ConduitT Command Command (ResourceT IO) ()
printCommands = mapMC
         (\x -> do
           liftIO $ hPutStrLn stderr ("Received command: " <> show x :: Text)
           pure x
         )

renderException :: SomeException -> Text
renderException e | Just (C.CppStdException m) <- fromException e = toS m
renderException e = toS $ displayException e

runCommands :: HerculesState -> ConduitM Command Event (ResourceT IO) ()
runCommands herculesState = do

  mainThread <- liftIO $ myThreadId

  awaitForever $ \case
    Command.Eval eval ->
      void $ liftIO $ flip forkFinally (\eeu -> case eeu of
        Left e -> throwIO $ FatalError $ "Failed to fork: " <> show e
        Right _ -> pure ()) $
        runConduitRes (
              Data.Conduit.handleC
                  (\e -> do
                    hPutStrLn stderr $ "Caught exception: " <> renderException e
                    yield $ Event.Error (renderException e)
                    liftIO $ throwTo mainThread (ExitFailure 1)
                  )
                  (do 
                    runEval herculesState eval
                    liftIO $ throwTo mainThread ExitSuccess
                  )
          .| sinkChan (shortcutChannel herculesState)
          )

    Command.BuildResult (BuildResult.BuildResult path result) -> do
      hPutStrLn stderr $ ("BuildResult: " <> show path <> " " <> show result :: Text)
      liftIO $ atomically $ modifyTVar (drvsCompleted herculesState) (<> M.singleton path result)

-- TODO: test
autoArgArgs :: Map Text Eval.Arg -> [ByteString]
autoArgArgs kvs = do
  (k, v) <- M.toList kvs
  case v of
    Eval.LiteralArg s -> ["--argstr", toS k, s]
    Eval.ExprArg s -> ["--arg", toS k, s]

withDrvInProgress :: HerculesState -> Text -> IO a -> IO a
withDrvInProgress HerculesState { drvsInProgress = ref } drvPath =
  bracket acquire release . const
    where
      acquire =
        join $ atomicModifyIORef ref $ \inprg ->
          if drvPath `S.member` inprg
            then (inprg, throwIO $ FatalError "Refusing to build derivation that should have been built remotely. Presumably, substitution has failed.")
            else (S.insert drvPath inprg, pass)
      release _ =
        atomicModifyIORef ref $ \inprg ->
           (S.delete drvPath inprg, ())

anyAlternative :: (Foldable l, Alternative f) => l a -> f a
anyAlternative = getAlt . foldMap (Alt . pure)

runEval :: HerculesState -> Eval -> ConduitM i Event (ResourceT IO) ()
runEval st@HerculesState {herculesStore = hStore, shortcutChannel = shortcutChan, drvsCompleted = drvsCompl} eval = do

  -- FIXME
  forM_ (Eval.extraNixOptions eval) $ liftIO . uncurry setGlobalOption

  do
    let store = nixStore hStore

    s <- storeUri store

    liftIO $ setBuilderCallback hStore $ \path -> do
      hPutStrLn stderr ("Building " <> show path :: Text)
      let (plainDrv, bangOut) = BS.span (/= fromIntegral (ord '!')) path
          outputName = BS.dropWhile (== fromIntegral (ord '!')) bangOut
          plainDrvText = toS plainDrv
      withDrvInProgress st plainDrvText $ do
        writeChan shortcutChan $ Just $ Event.Build plainDrvText
        -- TODO: try to fetch immediately
        hPutStrLn stderr ("Awaiting " <> show plainDrvText :: Text)
        result <- liftIO $ atomically $ do
          c <- readTVar drvsCompl
          anyAlternative $ M.lookup plainDrvText c

        case result of
          BuildResult.Exceptional msg -> throwIO $ FatalError $ "Technical failure in build of " <> plainDrvText <> ": " <> msg
          BuildResult.Failure -> throwIO $ FatalError $ "Build failed: " <> plainDrvText
          BuildResult.Success -> pass

        derivation <- getDerivation store plainDrv
        outputPath <- derivationOutputPath derivation outputName
        ensurePath (wrappedStore st) outputPath

      hPutStrLn stderr ("Built " <> show path :: Text)

    hPutStrLn stderr ("Store uri: " <> s)

    withEvalState store $ \evalState -> do

      hPutStrLn stderr ("EvalState loaded." :: Text)

      args <- liftIO
        $ evalArgs evalState (autoArgArgs (Eval.autoArguments eval))

      Data.Conduit.handleC
          (\e -> yield $ Event.AttributeError $ AttributeError.AttributeError
            { AttributeError.path = []
            , AttributeError.message = renderException e
            }
          )
        $ do
            imprt <- liftIO $ evalFile evalState (toS $ Eval.file eval)
            applied <- liftIO (autoCallFunction evalState imprt args)
            walk evalState args applied

      yield Event.EvaluationDone


walk :: Ptr EvalState
     -> Bindings
     -> RawValue
     -> ConduitT i Event (ResourceT IO) ()
walk evalState = walk' True [] 10
 where
  handleErrors path = Data.Conduit.handleC
    (\e -> yield $ Event.AttributeError $ AttributeError.AttributeError
      { AttributeError.path = path
      , AttributeError.message = renderException e
      }
    )

  walk' :: Bool                -- ^ If True, always walk this attribute set. Only True for the root.
        -> [ByteString]        -- ^ Attribute path
        -> Integer             -- ^ Depth of tree remaining
        -> Bindings            -- ^ Auto arguments to pass to (attrset-)functions
        -> RawValue               -- ^ Current node of the walk
        -> ConduitT i1 Event (ResourceT IO) () -- ^ Program that performs the walk and emits 'Event's
  walk' forceWalkAttrset path depthRemaining autoArgs v =
    -- liftIO $ hPutStrLn stderr $ "Walking " <> (show path :: Text)
    handleErrors path
      $ liftIO (match evalState v)
      >>= \case
            Left e ->
              yield $ Event.AttributeError $ AttributeError.AttributeError
                { AttributeError.path = path
                , AttributeError.message = renderException e
                }
            Right m -> case m of
              IsAttrs attrValue -> do
                isDeriv <- liftIO $ isDerivation evalState v
                if isDeriv
                  then do
                    drvPath <- getDrvFile evalState v
                    yield $ Event.Attribute Attribute.Attribute
                      { Attribute.path = path
                      , Attribute.drv = drvPath
                      }
                  else do
                    walkAttrset <- if forceWalkAttrset
                      then pure True
                      else
-- Hydra doesn't seem to obey this, because it walks the
-- x64_64-linux etc attributes per package. Maybe those
-- are special cases?
-- For now, we will assume that people don't build a whole Nixpkgs
                           liftIO $ getRecurseForDerivations evalState attrValue

                    isfunctor <- liftIO $ isFunctor evalState v
                    if isfunctor && walkAttrset
                      then do
                        x <- liftIO (autoCallFunction evalState v autoArgs)
                        walk' True path (depthRemaining - 1) autoArgs x
                      else do
                        attrs <- liftIO $ getAttrs attrValue

                        void
                          $ flip M.traverseWithKey attrs
                          $ \name value ->
                              when (depthRemaining > 0 && walkAttrset) $ -- TODO: else warn
                                                                         walk'
                                False
                                (path ++ [name])
                                (depthRemaining - 1)
                                autoArgs
                                value

              _any -> liftIO $ do
                vt <- rawValueType v
                unless
                    (last path
                    == "recurseForDerivations"
                    && vt
                    == CNix.Internal.Raw.Bool
                    )
                  $ hPutStrLn stderr
                  $ "Ignoring "
                  <> show path
                  <> " : "
                  <> (show vt :: Text)
                pass
