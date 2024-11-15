{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Agent.Config.BinaryCaches
  ( BinaryCaches (..),
    parseFile,
  )
where

import Control.Exception.Lifted (catchJust)
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Hercules.Agent.Bag
import Hercules.Agent.Config
import Hercules.Agent.Log
import Hercules.Error
import Hercules.Formats.CachixCache
import Hercules.Formats.NixCache
import Protolude hiding (catchJust)
import System.IO.Error (isDoesNotExistError)

data BinaryCaches = BinaryCaches
  { cachixCaches :: Map Text CachixCache,
    nixCaches :: Map Text NixCache,
    unknownKinds :: Map Text UnknownKind
  }

data UnknownKind = UnknownKind {kind :: Text}
  deriving (Generic, FromJSON)

instance FromJSON BinaryCaches where
  -- note that parseBag already preserves object names.
  parseJSON =
    parseBag
      ( BinaryCaches
          <$> part (\_name -> whenKind "CachixCache" $ \v -> Just $ parseJSON v)
          <*> part (\_name -> whenKind "NixCache" $ \v -> Just $ parseJSON v)
          <*> part (\_name v -> Just $ parseJSON v)
      )

parseFile :: Config 'Final -> KatipContextT IO BinaryCaches
parseFile cfg = do
  let fname = binaryCachesPath cfg
  bytes <-
    catchJust
      (mfilter isDoesNotExistError . Just)
      (liftIO (BL.readFile (toS fname)))
      ( \_e ->
          liftIO $
            throwIO $
              FatalError $
                "The binary-caches.json file does not exist.\
                \\nAccording to the configuration, it should be in\
                \\n  "
                  <> toS fname
                  <> "\
                     \\n\
                     \\nFor more information about binary-caches.json, see\n\
                     \\nhttps://docs.hercules-ci.com/hercules-ci/reference/binary-caches-json/"
      )
  bcs <- escalateAs (FatalError . toS) $ eitherDecode bytes
  validate (toS fname) bcs
  pure bcs

validate :: FilePath -> BinaryCaches -> KatipContextT IO ()
validate fname bcs = do
  when (null (cachixCaches bcs) && null (nixCaches bcs)) $
    logLocM
      WarningS
      "You did not configure any caches. This is ok for trying out Hercules CI,\
      \ but a cache _is_ required for multi-agent operation and\
      \ to work well across garbage collection.\
      \ For more information about binary-caches.json, see\
      \ https://docs.hercules-ci.com/hercules-ci/reference/binary-caches-json/"
  forM_ (M.toList (unknownKinds bcs)) $ \(k, v) ->
    logLocM WarningS $
      "In file "
        <> logStr fname
        <> " in entry "
        <> logStr k
        <> ": unknown kind "
        <> logStr (kind v)
