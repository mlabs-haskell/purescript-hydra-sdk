-- | This module provides an interface for spinning up a hydra-node
-- | as a Node.js child process.
module HydraSdk.Internal.Process.HydraNode
  ( HydraHeadPeer
  , HydraNodeHandlers
  , HydraNodeStartupParams
  , hydraHeadPeerCodec
  , hydraNodeStartupParamsCodec
  , noopHydraNodeHandlers
  , spawnHydraNode
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types (TransactionHash)
import Contract.CborBytes (cborBytesToHex)
import Control.Error.Util (bool)
import Data.Array (concat, singleton) as Array
import Data.Codec.Argonaut (JsonCodec, array, int, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Foldable (foldMap)
import Data.Int (decimal, toStringAs) as Int
import Data.Maybe (Maybe(Nothing), isNothing)
import Data.String (Pattern(Pattern))
import Data.String (contains) as String
import Data.Traversable (for_, traverse_)
import Effect (Effect)
import Effect.AVar (empty, tryPut) as AVar
import Effect.Class (class MonadEffect, liftEffect)
import HydraSdk.Internal.Lib.Codec (txHashCodec)
import HydraSdk.Internal.Types.HostPort
  ( HostPort
  , hostPortCodec
  , printHost
  , printHostPort
  , printPort
  )
import HydraSdk.Internal.Types.Network (Network(Testnet, Mainnet), networkCodec)
import Node.ChildProcess (ChildProcess, defaultSpawnOptions, spawn, stderr, stdout)
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.Path (FilePath)
import Node.Stream (onDataString)

-- | Parameters to be passed to the hydra-node child process on startup.
type HydraNodeStartupParams =
  { nodeId :: String
  , hydraNodeAddress :: HostPort
  , hydraNodeApiAddress :: HostPort
  , persistDir :: FilePath
  , hydraSigningKey :: FilePath
  , cardanoSigningKey :: FilePath
  , network :: Network
  , nodeSocket :: FilePath
  , pparams :: FilePath
  , hydraScriptsTxHash :: TransactionHash
  , contestPeriodSec :: Int
  , peers :: Array HydraHeadPeer
  }

-- | Bidirectional JSON codec for `HydraNodeStartupParams`.
hydraNodeStartupParamsCodec :: CA.JsonCodec HydraNodeStartupParams
hydraNodeStartupParamsCodec =
  CA.object "HydraNodeStartupParams" $ CAR.record
    { nodeId: CA.string
    , hydraNodeAddress: hostPortCodec
    , hydraNodeApiAddress: hostPortCodec
    , persistDir: CA.string
    , hydraSigningKey: CA.string
    , cardanoSigningKey: CA.string
    , network: networkCodec
    , nodeSocket: CA.string
    , pparams: CA.string
    , hydraScriptsTxHash: txHashCodec
    , contestPeriodSec: CA.int
    , peers: CA.array hydraHeadPeerCodec
    }

-- | Configuration parameters for a single Hydra Head peer. When setting up a
-- | Hydra Head, each node must specify the network addresses and public key
-- | information of its respective peers.
type HydraHeadPeer =
  { hydraNodeAddress :: HostPort
  , hydraVerificationKey :: FilePath
  , cardanoVerificationKey :: FilePath
  }

-- | Bi-directional JSON codec for `HydraHeadPeer`.
hydraHeadPeerCodec :: CA.JsonCodec HydraHeadPeer
hydraHeadPeerCodec =
  CA.object "HydraHeadPeer" $ CAR.record
    { hydraNodeAddress: hostPortCodec
    , hydraVerificationKey: CA.string
    , cardanoVerificationKey: CA.string
    }

-- | Optional handlers to attach to the newly spawned hydra-node child process.
type HydraNodeHandlers =
  { apiServerStartedHandler :: Maybe (Effect Unit)
  , stdoutHandler :: Maybe (String -> Effect Unit)
  , stderrHandler :: Maybe (String -> Effect Unit)
  }

-- | Record with no-op handlers, useful for specifying individual handlers
-- | with minimal code.
noopHydraNodeHandlers :: HydraNodeHandlers
noopHydraNodeHandlers =
  { apiServerStartedHandler: Nothing
  , stdoutHandler: Nothing
  , stderrHandler: Nothing
  }

-- | Launches hydra-node as a subprocess using the specified configuration.
-- |
-- | If provided, stdout and stderr handlers will be attached.
-- | Inspects the hydra-node stdout for the "APIServerStarted" message and
-- | executes the provided callback if set. This callback is guaranteed to be
-- | executed at most once. Typically, the "APIServerStarted" callback should be
-- | used to determine when a connection to the hydra-node API WebSocket can be
-- | established.
-- |
-- | NOTE: The hydra-node executable must be available in the PATH.
spawnHydraNode
  :: forall m
   . MonadEffect m
  => HydraNodeStartupParams
  -> HydraNodeHandlers
  -> m ChildProcess
spawnHydraNode params handlers = liftEffect do
  hydraNodeProcess <- spawn "hydra-node" hydraNodeArgs defaultSpawnOptions

  for_ handlers.stderrHandler \stderrHandler ->
    onDataString (stderr hydraNodeProcess) Encoding.UTF8 \str ->
      stderrHandler str

  unless (isNothing handlers.apiServerStartedHandler && isNothing handlers.stdoutHandler) do
    -- Binary semaphore used to prevent the callback from being invoked
    -- multiple times on API server startup.
    apiServerStartedSem <- AVar.empty

    onDataString (stdout hydraNodeProcess) Encoding.UTF8 \str -> do
      traverse_ (_ $ str) handlers.stdoutHandler
      for_ handlers.apiServerStartedHandler \apiServerStartedHandler ->
        when (String.contains (Pattern "APIServerStarted") str) $
          bool (pure unit) apiServerStartedHandler
            =<< AVar.tryPut unit apiServerStartedSem

  pure hydraNodeProcess
  where
  option :: String -> String -> Array String
  option name val = [ "--" <> name, val ]

  networkArgs :: Array String
  networkArgs =
    case params.network of
      Testnet { magic } ->
        option "testnet-magic" $ Int.toStringAs Int.decimal magic
      Mainnet ->
        Array.singleton "--mainnet"

  peerArgs :: Array String
  peerArgs =
    foldMap
      ( \peer -> Array.concat
          [ option "peer" $ printHostPort peer.hydraNodeAddress
          , option "hydra-verification-key" peer.hydraVerificationKey
          , option "cardano-verification-key" peer.cardanoVerificationKey
          ]
      )
      params.peers

  hydraNodeArgs :: Array String
  hydraNodeArgs =
    networkArgs <> peerArgs <> Array.concat
      [ option "node-id" params.nodeId
      , option "host" $ printHost params.hydraNodeAddress
      , option "port" $ printPort params.hydraNodeAddress
      , option "api-host" $ printHost params.hydraNodeApiAddress
      , option "api-port" $ printPort params.hydraNodeApiAddress
      , option "persistence-dir" params.persistDir
      , option "hydra-signing-key" params.hydraSigningKey
      , option "cardano-signing-key" params.cardanoSigningKey
      , option "node-socket" params.nodeSocket
      , option "ledger-protocol-parameters" params.pparams
      , option "hydra-scripts-tx-id" $ cborBytesToHex $ encodeCbor params.hydraScriptsTxHash
      , option "contestation-period" $ Int.toStringAs Int.decimal params.contestPeriodSec
      ]
