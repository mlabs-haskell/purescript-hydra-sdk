module HydraSdk.Internal.Types.ArgonautJson
  ( ArgonautJson(ArgonautJson)
  , argonautJsonCodec
  ) where

import Prelude

import Data.Argonaut (Json, stringify)
import Data.Codec.Argonaut (JsonCodec, json) as CA
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (wrapIso)

newtype ArgonautJson = ArgonautJson Json

derive instance Newtype ArgonautJson _
derive instance Eq ArgonautJson

instance Show ArgonautJson where
  show = stringify <<< unwrap

argonautJsonCodec :: CA.JsonCodec ArgonautJson
argonautJsonCodec = wrapIso ArgonautJson CA.json
