{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hercules.Agent.Config.Toml where

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as AK
import Data.Profunctor (Profunctor (..))
import Data.Scientific (floatingOrInteger, fromFloatDigits)
import Data.Vector qualified as V
import Protolude hiding (to)
import Toml

instance Profunctor Toml.Codec where
  dimap f g (Toml.Codec r w) = Toml.Codec (\x -> g <$> r x) (\a -> g <$> (w (f a)))

embedJson :: Key -> TomlCodec A.Value
embedJson key =
  Codec
    { codecRead =
        codecRead (Toml.match (embedJsonBiMap key) key)
          <!> codecRead (A.Object . AK.fromHashMapText <$> Toml.tableHashMap Toml._KeyText embedJson key),
      codecWrite = panic "embedJson.write: not implemented" $ \case
        A.String s -> A.String <$> codecWrite (Toml.text key) s
        A.Number sci -> A.Number . fromRational . toRational <$> codecWrite (Toml.double key) (fromRational $ toRational sci)
        A.Bool b -> A.Bool <$> codecWrite (Toml.bool key) b
        A.Array a -> A.Array . V.fromList <$> codecWrite (Toml.arrayOf (embedJsonBiMap key) key) (Protolude.toList a)
        A.Object o -> A.Object . AK.fromHashMapText <$> codecWrite (Toml.tableHashMap Toml._KeyText embedJson key) (AK.toHashMapText o)
        A.Null -> eitherToTomlState (Left ("null is not supported in TOML" :: Text))
    }

embedJsonBiMap :: Key -> TomlBiMap A.Value AnyValue
embedJsonBiMap _key =
  -- TODO: use key for error reporting
  BiMap
    { forward = panic "embedJsonBiMap.forward: not implemented" $ \case
        A.String s -> pure $ AnyValue $ Text s
        A.Number sci -> case floatingOrInteger sci of
          Left fl -> pure $ AnyValue $ Double fl -- lossy
          Right i -> pure $ AnyValue $ Integer i
        A.Bool b -> pure $ AnyValue $ Bool b
        A.Array _a -> Left $ ArbitraryError "Conversion from JSON array of arrays to TOML not implemented yet"
        A.Object _o -> Left $ ArbitraryError "Conversion from JSON array of objects to TOML is not supported"
        A.Null -> Left $ ArbitraryError "JSON null is not supported in TOML",
      backward = anyValueToJSON
    }

anyValueToJSON :: AnyValue -> Either TomlBiMapError A.Value
anyValueToJSON = \case
  AnyValue (Bool b) -> pure (A.Bool b)
  AnyValue (Integer i) -> pure (A.Number $ fromIntegral i)
  AnyValue (Double d) -> pure (A.Number $ fromFloatDigits d)
  AnyValue (Text t) -> pure (A.String t)
  AnyValue (Zoned _zt) -> Left (ArbitraryError "Conversion from TOML zoned time to JSON not implemented yet. Use a string.")
  AnyValue (Local _zt) -> Left (ArbitraryError "Conversion from TOML local time to JSON not implemented yet. Use a string.")
  AnyValue (Day _d) -> Left (ArbitraryError "Conversion from TOML day to JSON not implemented yet. Use a string.")
  AnyValue (Hours _h) -> Left (ArbitraryError "Conversion from TOML hours to JSON not implemented yet. Use a string.")
  AnyValue (Array a) -> A.Array <$> sequence (V.fromList (a <&> AnyValue <&> anyValueToJSON))
