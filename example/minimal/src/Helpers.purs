module HydraSdk.Example.Minimal.Helpers
  ( printHex
  ) where

import Prelude

import Cardano.AsCbor (class AsCbor, encodeCbor)
import Data.ByteArray (byteArrayToHex)
import Data.Newtype (unwrap)

printHex :: forall (a :: Type). AsCbor a => a -> String
printHex =
  byteArrayToHex
    <<< unwrap
    <<< encodeCbor
