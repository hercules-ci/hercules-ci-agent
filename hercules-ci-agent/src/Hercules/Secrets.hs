{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Hercules.Secrets
  ( SecretContext (..),
    evalCondition,
    evalConditionTrace,
  )
where

import qualified Control.Monad.Writer
import Data.Binary (Binary)
import Data.Tagged
import qualified Data.Text as T
import Hercules.Formats.Secret
import Protolude

data SecretContext = SecretContext
  { ownerName :: Text,
    repoName :: Text,
    isDefaultBranch :: Bool,
    ref :: Text
  }
  deriving (Generic, Binary, Show, Eq)

evalCondition' :: (Monad m, MonadMiniWriter [Text] m) => SecretContext -> Condition -> m Bool
evalCondition' ctx = eval
  where
    eval (Or cs) = do
      tell ["or: Entering"]
      let go [] = do
            tell ["or: Leaving (false)"]
            pure False
          go (a : as) = do
            b <- eval a
            if b
              then do
                tell ["or: Leaving (true)"]
                pure True
              else do
                unless (null as) (tell ["or: Backtracking"])
                go as
      go cs
    eval (And cs) = do
      tell ["and: Entering"]
      let go [] = do
            tell ["and: Leaving (true)"]
            pure True
          go (a : as) = do
            b <- eval a
            if b
              then go as
              else do
                tell ["and: Leaving (false)"]
                pure False
      go cs
    eval IsDefaultBranch =
      if isDefaultBranch ctx
        then pure True
        else False <$ tell ["isDefaultBranch: ref " <> show (ref ctx) <> " is not the default branch"]
    eval IsTag =
      if "refs/tags/" `T.isPrefixOf` ref ctx
        then pure True
        else False <$ tell ["isTag: ref " <> show (ref ctx) <> " is not a tag"]
    eval (IsBranch b) = do
      let expect = "refs/heads/" <> b
          actual = ref ctx
      if expect == actual
        then pure True
        else False <$ tell ["isBranch: ref " <> show actual <> " is not the desired " <> show expect]
    eval (IsRepo expect) = do
      let actual = repoName ctx
      if actual == expect
        then pure True
        else False <$ tell ["isRepo: repo " <> show actual <> " is not the desired " <> show expect]
    eval (IsOwner expect) = do
      let actual = ownerName ctx
      if actual == expect
        then pure True
        else False <$ tell ["isOwner: owner " <> show actual <> " is not the desired " <> show expect]

-- This uses tagless final to derive both an efficient and a tracing function.

evalCondition :: SecretContext -> Condition -> Bool
evalCondition ctx c = unTagged @[Text] @Bool (evalCondition' ctx c)

evalConditionTrace :: SecretContext -> Condition -> ([Text], Bool)
evalConditionTrace = evalCondition'

-- | Like 'Control.Monad.Class.Writer.MonadWriter' but simpler.
class MonadMiniWriter w m | m -> w where
  tell :: w -> m ()

instance Monoid w => MonadMiniWriter w ((,) w) where
  tell = Control.Monad.Writer.tell

instance MonadMiniWriter w (Tagged w) where
  tell _ = pure ()
