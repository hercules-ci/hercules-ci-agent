{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hercules.CNix.Std.String.Context
  ( CStdString,
    stdStringCtx,
  )
where

import qualified Data.Map as M
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types as C
import Prelude

data CStdString

stdStringCtx :: C.Context
stdStringCtx =
  C.cppCtx
    <> mempty
      { C.ctxTypesTable =
          M.singleton (C.TypeName "std::string") [t|CStdString|]
      }
