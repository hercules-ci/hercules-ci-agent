{-# LANGUAGE DeriveAnyClass #-}

module Hercules.API.Agent.Socket.Frame where

import Control.Applicative
import Data.Void (Void)
import Hercules.API.Prelude

-- | Adds serial number 'n' to payloads 'p' and allows acknowledgement for
-- a related stream that travels in the opposite direction.
data Frame o a
  = -- | Message
    Msg {n :: Integer, p :: a}
  | -- | Out of band message: not redelivered, not acknowledged.
    Oob {o :: o}
  | -- | Acknowledgement
    Ack {n :: Integer}
  | -- | Exception
    Exception {message :: Text}
  deriving (Generic, Show, Eq, NFData, ToJSON, FromJSON)

mapOob :: (a -> b) -> Frame a c -> Frame b c
mapOob f (Oob a) = Oob (f a)
mapOob _ (Msg a b) = Msg a b
mapOob _ (Ack a) = Ack a
mapOob _ (Exception e) = Exception e

removeOob :: Alternative f => Frame o a -> f (Frame Void a)
removeOob (Msg a b) = pure $ Msg a b
removeOob (Ack a) = pure $ Ack a
removeOob Oob {} = empty
removeOob (Exception e) = pure $ Exception e
