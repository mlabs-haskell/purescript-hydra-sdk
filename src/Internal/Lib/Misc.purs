module HydraSdk.Internal.Lib.Misc
  ( cborBytesToHex
  , concatPathSegments
  ) where

import Prelude

import Cardano.Types (CborBytes)
import Data.ByteArray (byteArrayToHex)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(Pattern), null, stripPrefix, stripSuffix) as String

cborBytesToHex :: CborBytes -> String
cborBytesToHex = byteArrayToHex <<< unwrap

-- | Concat two strings with "/" in the middle, but stripping multiple slashes.
concatPathSegments :: String -> String -> String
concatPathSegments a b =
  if String.null right then left
  else left <> "/" <> right
  where
  left = fromMaybe a (String.stripSuffix (String.Pattern "/") a)
  right = fromMaybe b (String.stripPrefix (String.Pattern "/") b)
