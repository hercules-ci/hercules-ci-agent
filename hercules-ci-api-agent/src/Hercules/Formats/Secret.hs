{-# LANGUAGE DeriveAnyClass #-}

module Hercules.Formats.Secret where

import Data.Aeson
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AM
import qualified Data.Aeson.Types as A
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import Hercules.API.Prelude
import Hercules.Formats.Common
  ( noVersion,
    withKind,
    withVersions,
  )

data Condition
  = Or [Condition]
  | And [Condition]
  | IsDefaultBranch
  | IsBranch Text
  | IsTag
  | IsRepo Text
  | IsOwner Text
  deriving (Generic, Eq, Read, Show)

instance ToJSON Condition where
  toJSON (Or a) = object ["or" .= a]
  toJSON (And a) = object ["and" .= a]
  toJSON IsDefaultBranch = String "isDefaultBranch"
  toJSON IsTag = String "isTag"
  toJSON (IsBranch a) = object ["isBranch" .= a]
  toJSON (IsRepo a) = object ["isRepo" .= a]
  toJSON (IsOwner a) = object ["isOwner" .= a]

instance FromJSON Condition where
  parseJSON (String "isTag") = pure IsTag
  parseJSON (String "isDefaultBranch") = pure IsDefaultBranch
  parseJSON (Object o) =
    case AM.toList o of
      [] -> fail "The empty object does not represent a Condition."
      [(k, v)] -> case HM.lookup (AK.toText k) taggedConditionParsers of
        Nothing -> fail $ "The field name in a Condition object must be one of " <> show (map fst (HM.toList taggedConditionParsers))
        Just p -> p v
      _ -> fail "A Condition object must contain a single field."
  parseJSON _ = fail "Expected Object or String."

taggedConditionParsers :: HM.HashMap Text (Value -> A.Parser Condition)
taggedConditionParsers =
  HM.fromList
    [ ( "or",
        \v -> do
          params <- parseJSON v
          Or <$> traverse parseJSON params
      ),
      ( "and",
        \v -> do
          params <- parseJSON v
          And <$> traverse parseJSON params
      ),
      ("isBranch", fmap IsBranch . parseJSON),
      ("isRepo", fmap IsRepo . parseJSON),
      ("isOwner", fmap IsOwner . parseJSON)
    ]

{-

This was awful:

instance FromJSON Condition where
  parseJSON (String "isTag") = pure IsTag
  parseJSON (String "isDefaultBranch") = pure IsDefaultBranch
  parseJSON j = do
    l <- parseJSON j
    case l of
      [] -> fail "The empty list does not represent a Condition."
      (jTag : args) -> do
        tag <- parseJSON jTag
        case tag :: Text of
          "or" -> do
            Or <$> traverse parseJSON args
          "and" -> do
            And <$> traverse parseJSON args
          "isDefaultBranch" -> do
            IsDefaultBranch <$ noParams tag args
          "isTag" -> do
            IsTag <$ noParams tag args
          "isBranch" -> do
            IsBranch <$> traverse parseJSON args
          "isRepo" -> do
            IsRepo <$> traverse parseJSON args
          "isOwner" -> do
            IsOwner <$> traverse parseJSON args
          t -> do
            fail $ "Unknown tag " <> show t <> " in Condition."
    where
      noParams _ [] = pure ()
      noParams tag _ = fail $ "Condition with tag " <> show tag <> " does not take any parameters."

instance ToJSON Condition where
  toJSON (Or a) = toJSON (String "or" : map toJSON a)
  toJSON (And a) = toJSON (String "and" : map toJSON a)
  toJSON IsDefaultBranch = String "isDefaultBranch"
  toJSON IsTag = String "isTag"
  toJSON (IsBranch a) = toJSON (String "isBranch" : map toJSON a)
  toJSON (IsRepo a) = toJSON (String "isRepo" : map toJSON a)
  toJSON (IsOwner a) = toJSON (String "isOwner" : map toJSON a)
-}

-- | Arbitrary secret like keys, tokens, passwords etc.
data Secret = Secret
  { data_ :: Map Text Value,
    condition :: Maybe Condition
  }

instance ToJSON Secret where
  toJSON a =
    object
      (["kind" .= String "Secret", "data" .= data_ a] ++ ["condition" .= x | x <- toList (condition a)])

  toEncoding a =
    pairs
      ("kind" .= String "Secret" <> "data" .= data_ a <> foldMap ("condition" .=) (condition a))

instance FromJSON Secret where
  parseJSON =
    withKind "Secret" $
      withVersions
        [ noVersion $ \o ->
            Secret
              <$> o .: "data"
              <*> o .:? "condition"
        ]
