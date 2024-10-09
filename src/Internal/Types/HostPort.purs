module HydraSdk.Internal.Types.HostPort
  ( HostPort
  , printHost
  , printHostPort
  , printPort
  ) where

import Prelude

import Data.Int (decimal, toStringAs) as Int
import URI (Host, Port)
import URI.Host (print) as Host
import URI.Port (toInt) as Port

type HostPort = { host :: Host, port :: Port }

printHost :: HostPort -> String
printHost = Host.print <<< _.host

printPort :: HostPort -> String
printPort = Int.toStringAs Int.decimal <<< Port.toInt <<< _.port

printHostPort :: HostPort -> String
printHostPort hp = printHost hp <> ":" <> printPort hp
