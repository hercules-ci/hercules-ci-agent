{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Define instances for C++ types in the Context module that can't be in that
-- module because of TH staging restrictions.
module Hercules.CNix.Std.String.Instances where

import Data.Semigroup (Semigroup ((<>)))
import Hercules.CNix.Std.Set
import Hercules.CNix.Std.String.Context
import Hercules.CNix.Std.Vector
import qualified Language.C.Inline.Cpp as C

C.context (stdVectorCtx <> stdSetCtx <> stdStringCtx)
C.include "<string>"

instanceStdVector "std::string"
instanceStdSet "std::string"
