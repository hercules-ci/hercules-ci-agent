{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Define instances for C++ types in the Context module that can't be in that
-- module because of TH staging restrictions.
module Hercules.CNix.Store.Instances () where

import Data.Data (Proxy (Proxy))
import Data.Function (($))
import Data.Semigroup (Semigroup ((<>)))
import Hercules.CNix.Std.Set
import Hercules.CNix.Std.Vector
import Hercules.CNix.Store.Context
import qualified Language.C.Inline.Cpp as C

C.context $ context <> stdVectorCtx <> stdSetCtx

#if NIX_IS_AT_LEAST(2, 28, 0)
C.include "<nix/store/path.hh>"
C.include "<nix/store/derivations.hh>"
C.include "<nix/store/path-with-outputs.hh>"
#else
C.include "<nix/path.hh>"
C.include "<nix/derivations.hh>"
C.include "<nix/path-with-outputs.hh>"
#endif

_ = Proxy :: Proxy NixStorePath

instanceStdVector "nix::StorePath *"
instanceStdVectorCopyable "nix::StorePath *"
instanceStdSet "nix::StorePath"

instanceStdVector "nix::StorePathWithOutputs *"
instanceStdVector "nix::StorePathWithOutputs"
