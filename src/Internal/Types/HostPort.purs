module HydraSdk.Internal.Types.HostPort
  ( HostPort
  , hostPortCodec
  , hostPortParser
  , printHost
  , printHostPort
  , printPort
  , readHostPort
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, prismaticCodec, string) as CA
import Data.Either (hush)
import Data.Int (decimal, toStringAs) as Int
import Data.Maybe (Maybe)
import Parsing (Parser, runParser)
import URI (Host, Port)
import URI.Host (parser, print) as Host
import URI.Port (parser, toInt) as Port

type HostPort = { host :: Host, port :: Port }

hostPortCodec :: CA.JsonCodec HostPort
hostPortCodec = CA.prismaticCodec "HostPort" readHostPort printHostPort CA.string

printHost :: HostPort -> String
printHost = Host.print <<< _.host

printPort :: HostPort -> String
printPort = Int.toStringAs Int.decimal <<< Port.toInt <<< _.port

printHostPort :: HostPort -> String
printHostPort hp = printHost hp <> ":" <> printPort hp

hostPortParser :: Parser String HostPort
hostPortParser = { host: _, port: _ } <$> Host.parser <*> Port.parser

readHostPort :: String -> Maybe HostPort
readHostPort = hush <<< flip runParser hostPortParser
