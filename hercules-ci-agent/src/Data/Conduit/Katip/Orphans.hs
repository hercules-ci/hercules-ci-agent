{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Conduit.Katip.Orphans where

import Control.Monad.Trans
import Data.Conduit
import Katip

instance Katip m => Katip (ConduitT i o m) where

  getLogEnv = lift getLogEnv

  localLogEnv f = transPipe (localLogEnv f)

instance KatipContext m => KatipContext (ConduitT i o m) where

  getKatipContext = lift getKatipContext

  getKatipNamespace = lift getKatipNamespace

  localKatipContext f = transPipe (localKatipContext f)

  localKatipNamespace f = transPipe (localKatipNamespace f)
