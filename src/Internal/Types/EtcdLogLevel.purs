module HydraSdk.Internal.Types.EtcdLogLevel
  ( EtcdLogLevel
      ( Debug
      , Info
      , Warn
      , Error
      , Panic
      , Fatal
      )
  , etcdLogLevelCodec
  , printEtcdLogLevel
  , readEtcdLogLevel
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, prismaticCodec, string) as CA
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)

data EtcdLogLevel
  = Debug
  | Info
  | Warn
  | Error
  | Panic
  | Fatal

derive instance Generic EtcdLogLevel _
derive instance Eq EtcdLogLevel

instance Show EtcdLogLevel where
  show = genericShow

etcdLogLevelCodec :: CA.JsonCodec EtcdLogLevel
etcdLogLevelCodec =
  CA.prismaticCodec "EtcdLogLevel" readEtcdLogLevel printEtcdLogLevel
    CA.string

readEtcdLogLevel :: String -> Maybe EtcdLogLevel
readEtcdLogLevel = case _ of
  "debug" -> Just Debug
  "info" -> Just Info
  "warn" -> Just Warn
  "error" -> Just Error
  "panic" -> Just Panic
  "fatal" -> Just Fatal
  _ -> Nothing

printEtcdLogLevel :: EtcdLogLevel -> String
printEtcdLogLevel = case _ of
  Debug -> "debug"
  Info -> "info"
  Warn -> "warn"
  Error -> "error"
  Panic -> "panic"
  Fatal -> "fatal"
