{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Formats.Common where

import Control.Monad
import Data.Aeson
  ( (.:),
    (.:?)
    )
import Data.Aeson.Types
  ( Object,
    Parser,
    Value,
    withObject
    )
import Data.Coerce (coerce)
import qualified Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

withKind :: Text -> (Object -> Parser a) -> Value -> Parser a
withKind k f = withObject (T.unpack k) $ \o -> do
  k' <- o .: "kind"
  when (k' /= k) $ fail $ "kind field must be " <> show k <> ", not " <> show k'
  f o

newtype VersionParser a
  = VersionParser
      { fromVersionParser :: (Maybe Text, Object -> Parser a)
        }
  deriving (Functor)

noVersion :: (Object -> Parser a) -> VersionParser a
noVersion = VersionParser . (Nothing,)

version :: Text -> (Object -> Parser a) -> VersionParser a
version t p = VersionParser (Just t, p)

withVersions :: forall a. [VersionParser a] -> Object -> Parser a
withVersions vps' o = do
  let vps = coerce vps' :: [(Maybe Text, Object -> Parser a)]
  v <- o .:? "apiVersion"
  case Data.List.lookup v vps of
    Just p -> p o
    Nothing ->
      let vs = map fst vps
       in case vs of
            [Nothing] -> fail "Unexpected apiVersion field. "
            _ ->
              fail $ "Expected apiVersion to be one of "
                <> unwords
                     (map showVersion vs)
              where
                showVersion Nothing = "<no version field>"
                showVersion (Just t) = show t
