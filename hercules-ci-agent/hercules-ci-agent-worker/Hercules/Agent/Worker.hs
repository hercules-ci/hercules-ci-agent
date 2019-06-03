{-# LANGUAGE DataKinds #-}

module Hercules.Agent.Worker
  ( main
  )
where

import           Prelude                        ( )
import           Control.Monad
import           Protolude               hiding ( evalState )
import           Data.Conduit.Serialization.Binary
                                                ( conduitEncode
                                                , conduitDecode
                                                )
import           Conduit
import qualified Data.Conduit
import qualified Data.Map                      as M
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
import           Hercules.Agent.WorkerProtocol.Event
                                                ( Event )
import qualified Language.C.Inline.Cpp.Exceptions
                                               as C

import           CNix
import qualified CNix.Internal.Raw
import           Data.List                      ( last )

main :: IO ()
main = do -- runInBoundThread $ do
  CNix.init

  runConduitRes
    (sourceHandle stdin
    .| conduitDecode
    .| mapMC
         (\x -> do
           liftIO $ hPutStrLn stderr ("Received command: " <> show x :: Text)
           pure x
         )
    .| runCommands
    .| conduitEncode
    .| concatMapC (\x -> [Chunk x, Flush])
    .| sinkHandleFlush stdout
    )

renderException :: SomeException -> Text
renderException e | Just (C.CppStdException m) <- fromException e = toS m
renderException e = toS $ displayException e

runCommands :: ConduitM Command Event (ResourceT IO) ()
runCommands = do
  let peekLoop m = peekC >>= \case
        Just a -> m a
        Nothing -> pass
  peekLoop $ \case
    Command.Eval eval ->
      Data.Conduit.handleC
          (\e -> do
            hPutStrLn stderr $ "Caught exception: " <> renderException e
            yield $ Event.Error (renderException e)
          )
        $ runEval eval
    Command.Build impossible -> absurd impossible

-- TODO: test
autoArgArgs :: Map Text Eval.Arg -> [ByteString]
autoArgArgs kvs = do
  (k, v) <- M.toList kvs
  case v of
    Eval.LiteralArg s -> ["--argstr", toS k, s]
    Eval.ExprArg s -> ["--arg", toS k, s]

runEval :: Eval -> ConduitM i Event (ResourceT IO) ()
runEval eval = do

  forM_ (Eval.extraNixOptions eval) $ liftIO . uncurry setGlobalOption

  hPutStrLn stderr ("Initializing store and evaluator..." :: Text)

  withStore $ \store -> do
    s <- storeUri store

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
