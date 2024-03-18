{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hercules.Agent.Config.Combined where

import Data.Bifunctor.Product (Product (Pair))
import Data.Profunctor (Profunctor (..))
import Hercules.Agent.Config.Json (JsonCodec)
import Hercules.Agent.Config.Json qualified as Json
import Hercules.Agent.Config.Toml ()
import Protolude
import Toml (Key)
import Toml qualified

newtype Combi a b = Combi (Data.Bifunctor.Product.Product (Toml.Codec) JsonCodec a b)
  deriving newtype (Functor, Profunctor)

type Combi' a = Combi a a

combi :: Toml.Codec a b -> JsonCodec a b -> Combi a b
combi toml json = Combi $ Pair toml json

forJson :: Combi a b -> JsonCodec a b
forJson (Combi (Data.Bifunctor.Product.Pair _ b)) = b

forToml :: Combi a b -> Toml.Codec a b
forToml (Combi (Data.Bifunctor.Product.Pair a _)) = a

instance Applicative (Combi a) where
  pure a = Combi (Data.Bifunctor.Product.Pair (pure a) (pure a))
  Combi (Data.Bifunctor.Product.Pair f g) <*> Combi (Data.Bifunctor.Product.Pair a b) = Combi (Data.Bifunctor.Product.Pair (f <*> a) (g <*> b))

textAtKey :: Key -> Combi' Text
textAtKey k = Combi (Data.Bifunctor.Product.Pair (Toml.text k) (Json.text k))

stringAtKey :: Key -> Combi' [Char]
stringAtKey k = Combi (Data.Bifunctor.Product.Pair (Toml.string k) (Json.string k))

boolAtKey :: Key -> Combi' Bool
boolAtKey k = Combi (Data.Bifunctor.Product.Pair (Toml.bool k) (Json.bool k))

opt :: Combi' a -> Combi' (Maybe a)
opt x = Combi $ Pair (Toml.dioptional (forToml x)) (Json.proOptional (forJson x))

optEmpty :: (Monoid a) => Combi' a -> Combi' a
optEmpty c = dimap Just (fromMaybe mempty) (opt c)

enumBoundedAtKey :: (Bounded a, Enum a, Show a) => Key -> Combi' a
enumBoundedAtKey k = Combi (Data.Bifunctor.Product.Pair (Toml.enumBounded k) (Json.enumBounded k))

tableMap :: (Key -> Combi' a) -> Key -> Combi' (Map Text a)
tableMap c k = Combi $ Pair (Toml.tableMap Toml._KeyText (forToml . c) k) (Json.tableMap (forJson . c) k)
