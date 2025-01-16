module HydraSdk.Internal.Types.Network
  ( Network(Testnet, Mainnet)
  , networkCodec
  , networkToNetworkId
  ) where

import Prelude

import Cardano.Types (NetworkId(TestnetId, MainnetId))
import Data.Codec.Argonaut (JsonCodec, int) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Network = Testnet { magic :: Int } | Mainnet

derive instance Generic Network _

instance Show Network where
  show = genericShow

networkCodec :: CA.JsonCodec Network
networkCodec =
  CAS.sumFlat "Network"
    { "Testnet": CAR.record { magic: CA.int }
    , "Mainnet": unit
    }

networkToNetworkId :: Network -> NetworkId
networkToNetworkId = case _ of
  Testnet _ -> TestnetId
  Mainnet -> MainnetId
