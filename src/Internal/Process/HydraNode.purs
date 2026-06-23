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
import Control.Error.Util (bool)
import Data.Array (catMaybes, concat, singleton) as Array
import Data.Bitraversable (rtraverse)
import Data.Codec.Argonaut (JsonCodec, JPropCodec, array, int, object, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Foldable (foldMap)
import Data.Int (decimal, toStringAs) as Int
import Data.Maybe (Maybe(Just, Nothing), isNothing, maybe)
import Data.String (Pattern(Pattern))
import Data.String (contains, joinWith) as String
import Data.Traversable (for_, traverse_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.AVar (empty, tryPut) as AVar
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object (fromFoldable, union) as Object
import HydraSdk.Internal.Lib.Codec (txHashCodec, unionRecordCodecs)
import HydraSdk.Internal.Lib.Misc (cborBytesToHex)
import HydraSdk.Internal.Types.EtcdLogLevel
  ( EtcdLogLevel
  , etcdLogLevelCodec
  , printEtcdLogLevel
  )
import HydraSdk.Internal.Types.HostPort
  ( HostPort
  , hostPortStringCodec
  , printHost
  , printHostPort
  , printPort
  )
import HydraSdk.Internal.Types.Network (Network(Testnet, Mainnet))
import HydraSdk.Internal.Types.QueryLayer
  ( QueryLayer(CardanoNode, Blockfrost)
  , queryLayerCodec
  )
import Node.ChildProcess
  ( ChildProcess
  , StdIOBehaviour(Pipe, Ignore)
  , defaultSpawnOptions
  , spawn
  , stderr
  , stdout
  )
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.Path (FilePath)
import Node.Process (getEnv)
import Node.Stream (onDataString)
import Prim.Row (class Union) as Row
import Prim.RowList (RowList)
import Prim.RowList (class RowToList) as RowList
import Record.Extra (class Keys) as Record.Extra

-- | Parameters to be passed to the hydra-node child process on startup.
type HydraNodeStartupParams (peerExtra :: Row Type) =
  { nodeId :: String
  , hydraNodeAddress :: HostPort
  , hydraNodeAdvertisedAddress :: Maybe HostPort
  , hydraNodeApiAddress :: HostPort
  , persistDir :: FilePath
  , hydraSigningKey :: FilePath
  , cardanoSigningKey :: FilePath
  , queryLayer :: QueryLayer
  , pparams :: FilePath
  , hydraScripts :: Array TransactionHash
  , contestPeriodSec :: Maybe Int
  , depositPeriodSec :: Maybe Int
  , unsyncedPeriodSec :: Maybe Int
  , peers :: Array (Record (HydraHeadPeer peerExtra))
  , etcd ::
      { logLevel :: Maybe EtcdLogLevel
      , logOutputs :: Maybe (Array String)
      }
  }

-- | Bidirectional JSON codec for `HydraNodeStartupParams`.
hydraNodeStartupParamsCodec
  :: forall (peerExtra :: Row Type) (rl :: RowList Type)
   . Row.Union peerExtra (HydraHeadPeer ()) (HydraHeadPeer peerExtra)
  => RowList.RowToList peerExtra rl
  => Record.Extra.Keys rl
  => CA.JPropCodec (Record peerExtra)
  -> CA.JsonCodec (HydraNodeStartupParams peerExtra)
hydraNodeStartupParamsCodec peerExtraCodec =
  CA.object "HydraNodeStartupParams" $ CAR.record
    { nodeId: CA.string
    , hydraNodeAddress: hostPortStringCodec
    , hydraNodeAdvertisedAddress: CA.maybe hostPortStringCodec
    , hydraNodeApiAddress: hostPortStringCodec
    , persistDir: CA.string
    , hydraSigningKey: CA.string
    , cardanoSigningKey: CA.string
    , queryLayer: queryLayerCodec
    , pparams: CA.string
    , hydraScripts: CA.array txHashCodec
    , contestPeriodSec: CA.maybe CA.int
    , depositPeriodSec: CA.maybe CA.int
    , unsyncedPeriodSec: CA.maybe CA.int
    , peers: CA.array $ hydraHeadPeerCodec peerExtraCodec
    , etcd:
        CA.object "HydraNodeStartupParams:etcd" $ CAR.record
          { logLevel: CA.maybe etcdLogLevelCodec
          , logOutputs: CA.maybe $ CA.array CA.string
          }
    }

-- | Configuration parameters for a single Hydra Head peer. When setting up a
-- | Hydra Head, each node must specify the network addresses and public key
-- | information of its respective peers.
type HydraHeadPeer (extra :: Row Type) =
  ( hydraNodeAddress :: HostPort
  , hydraVerificationKey :: FilePath
  , cardanoVerificationKey :: FilePath
  | extra
  )

-- | Bi-directional JSON codec for `HydraHeadPeer`.
hydraHeadPeerCodec
  :: forall (extra :: Row Type) (rl :: RowList Type)
   . Row.Union extra (HydraHeadPeer ()) (HydraHeadPeer extra)
  => RowList.RowToList extra rl
  => Record.Extra.Keys rl
  => CA.JPropCodec (Record extra)
  -> CA.JsonCodec (Record (HydraHeadPeer extra))
hydraHeadPeerCodec extraCodec =
  CA.object "HydraHeadPeer" $ unionRecordCodecs extraCodec $ CAR.record
    { hydraNodeAddress: hostPortStringCodec
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
  :: forall (m :: Type -> Type) (peerExtra :: Row Type)
   . MonadEffect m
  => HydraNodeStartupParams peerExtra
  -> HydraNodeHandlers
  -> m ChildProcess
spawnHydraNode params handlers = liftEffect do
  env <- getEnv
  hydraNodeProcess <- spawn "hydra-node" hydraNodeArgs $ defaultSpawnOptions
    { stdio =
        [ Just Ignore
        , Just Pipe
        , Just Pipe
        ]
    , env =
        Just $ flip Object.union env $ Object.fromFoldable $ Array.catMaybes
          ( rtraverse identity <$>
              -- https://etcd.io/docs/v3.4/op-guide/configuration
              [ "ETCD_LOG_LEVEL" /\ (printEtcdLogLevel <$> params.etcd.logLevel)
              , "ETCD_LOG_OUTPUTS" /\ (String.joinWith "," <$> params.etcd.logOutputs)
              ]
          )
    }

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

  optionMaybe :: String -> Maybe String -> Array String
  optionMaybe name = maybe mempty (option name)

  networkArgs :: Network -> Array String
  networkArgs =
    case _ of
      Testnet { magic } ->
        option "testnet-magic" $ Int.toStringAs Int.decimal magic
      Mainnet ->
        Array.singleton "--mainnet"

  queryLayerArgs :: Array String
  queryLayerArgs =
    case params.queryLayer of
      CardanoNode { nodeSocket, network } ->
        networkArgs network
          <> option "node-socket" nodeSocket
      Blockfrost { apiKeyFile, queryTimeoutSec, retryTimeoutSec } ->
        Array.concat
          [ option "blockfrost" apiKeyFile
          , optionMaybe "blockfrost-query-timeout" $ show <$> queryTimeoutSec
          , optionMaybe "blockfrost-retry-timeout" $ show <$> retryTimeoutSec
          ]

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
    queryLayerArgs <> peerArgs <> Array.concat
      [ option "node-id" params.nodeId
      , option "listen" $ printHostPort params.hydraNodeAddress
      , optionMaybe "advertise" $ printHostPort <$> params.hydraNodeAdvertisedAddress
      , option "api-host" $ printHost params.hydraNodeApiAddress
      , option "api-port" $ printPort params.hydraNodeApiAddress
      , option "persistence-dir" params.persistDir
      , option "hydra-signing-key" params.hydraSigningKey
      , option "cardano-signing-key" params.cardanoSigningKey
      , option "ledger-protocol-parameters" params.pparams
      , option "hydra-scripts-tx-id" $
          String.joinWith "," (cborBytesToHex <<< encodeCbor <$> params.hydraScripts)
      , optionMaybe "contestation-period" $ toSeconds <$> params.contestPeriodSec
      , optionMaybe "deposit-period" $ toSeconds <$> params.depositPeriodSec
      , optionMaybe "unsynced-period" $ toSeconds <$> params.unsyncedPeriodSec
      ]

  toSeconds :: Int -> String
  toSeconds x = Int.toStringAs Int.decimal x <> "s"
