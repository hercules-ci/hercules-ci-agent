{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.CNix.Verbosity
  ( Verbosity (..),
    setVerbosity,
    getVerbosity,
    setShowTrace,
    getShowTrace,
  )
where

import Foreign (fromBool, toBool)
import Hercules.CNix.Store.Context (context)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Protolude

C.context context

C.include "<nix/config.h>"
C.include "<nix/error.hh>"
C.include "<nix/globals.hh>"
C.include "<nix/logging.hh>"

data Verbosity
  = Error
  | Warn
  | Notice
  | Info
  | Talkative
  | Chatty
  | Debug
  | Vomit
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

setVerbosity :: Verbosity -> IO ()
setVerbosity Error = [C.throwBlock| void { nix::verbosity = nix::lvlError; } |]
setVerbosity Warn = [C.throwBlock| void { nix::verbosity = nix::lvlWarn; } |]
setVerbosity Notice = [C.throwBlock| void { nix::verbosity = nix::lvlNotice; } |]
setVerbosity Info = [C.throwBlock| void { nix::verbosity = nix::lvlInfo; } |]
setVerbosity Talkative = [C.throwBlock| void { nix::verbosity = nix::lvlTalkative; } |]
setVerbosity Chatty = [C.throwBlock| void { nix::verbosity = nix::lvlChatty; } |]
setVerbosity Debug = [C.throwBlock| void { nix::verbosity = nix::lvlDebug; } |]
setVerbosity Vomit = [C.throwBlock| void { nix::verbosity = nix::lvlVomit; } |]

getVerbosity :: IO Verbosity
getVerbosity =
  [C.throwBlock| int { switch(nix::verbosity) {
    case nix::lvlError: return 1;
    case nix::lvlWarn: return 2;
    case nix::lvlNotice: return 3;
    case nix::lvlInfo: return 4;
    case nix::lvlTalkative: return 5;
    case nix::lvlChatty: return 6;
    case nix::lvlDebug: return 7;
    case nix::lvlVomit: return 8;
    default: return 0;
  } }|]
    >>= \case
      1 -> pure Error
      2 -> pure Warn
      3 -> pure Notice
      4 -> pure Info
      5 -> pure Talkative
      6 -> pure Chatty
      7 -> pure Debug
      8 -> pure Vomit
      _ -> throwIO (FatalError "Unknown nix::verbosity value")

getShowTrace :: IO Bool
getShowTrace =
  [C.throwBlock| bool { return nix::loggerSettings.showTrace.get(); }|]
    <&> toBool

setShowTrace :: Bool -> IO ()
setShowTrace b =
  let b' = fromBool b
   in [C.throwBlock| void { nix::loggerSettings.showTrace.assign($(bool b')); }|]
