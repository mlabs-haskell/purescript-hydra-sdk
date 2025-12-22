module HydraSdk.Internal.Types.HostPort
  ( HostPort
  , hostCodec
  , hostPortObjectCodec
  , hostPortOption
  , hostPortParser
  , hostPortStringCodec
  , portCodec
  , printHost
  , printHostPort
  , printPort
  , readHostPort
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, int, object, prismaticCodec, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (hush)
import Data.Int (decimal, toStringAs) as Int
import Data.Maybe (Maybe)
import HydraSdk.Internal.Lib.Optparse (parserReader)
import Options.Applicative (ReadM) as Optparse
import Parsing (Parser, runParser)
import URI (Host, Port)
import URI.Host (parser, print) as Host
import URI.Port (fromInt, parser, toInt) as Port

type HostPort = { hostname :: Host, port :: Port }

hostPortStringCodec :: CA.JsonCodec HostPort
hostPortStringCodec =
  CA.prismaticCodec "HostPort:str" readHostPort printHostPort
    CA.string

hostPortObjectCodec :: CA.JsonCodec HostPort
hostPortObjectCodec =
  CA.object "HostPort:obj" $ CAR.record
    { hostname: hostCodec
    , port: portCodec
    }

hostCodec :: CA.JsonCodec Host
hostCodec =
  CA.prismaticCodec "Host" (hush <<< flip runParser Host.parser) Host.print
    CA.string

portCodec :: CA.JsonCodec Port
portCodec =
  CA.prismaticCodec "Port" Port.fromInt Port.toInt
    CA.int

hostPortOption :: Optparse.ReadM HostPort
hostPortOption = parserReader "HostPort" hostPortParser

printHost :: HostPort -> String
printHost = Host.print <<< _.hostname

printPort :: HostPort -> String
printPort = Int.toStringAs Int.decimal <<< Port.toInt <<< _.port

printHostPort :: HostPort -> String
printHostPort hp = printHost hp <> ":" <> printPort hp

hostPortParser :: Parser String HostPort
hostPortParser = { hostname: _, port: _ } <$> Host.parser <*> Port.parser

readHostPort :: String -> Maybe HostPort
readHostPort = hush <<< flip runParser hostPortParser
