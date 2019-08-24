module Hercules.Agent.StoreFFI where

import Protolude
import qualified Foreign.C
import           Foreign.ForeignPtr


type BuilderCallback = Foreign.C.CString -> Ptr ExceptionPtr -> IO ()

foreign import ccall "wrapper"
  mkBuilderCallback :: BuilderCallback -> IO (FunPtr BuilderCallback)

data ExceptionPtr
