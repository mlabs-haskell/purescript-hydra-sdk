module HydraSdk.Internal.Test.Cluster
  ( HydraClusterSpec
  , HydraClusterTimeParams
  , HydraPeerSpec
  , defaultHydraClusterTimeParamsForCardanoTestnet
  , genHydraNodeConfigurations
  ) where

import Prelude

import Aeson (encodeAeson, stringifyAeson)
import Cardano.AsCbor (decodeCbor)
import Cardano.Types (PrivateKey, PublicKey, TransactionHash)
import Cardano.Types.PrivateKey (toPublicKey, toRawBytes) as PrivateKey
import Cardano.Types.PublicKey (toRawBytes) as PublicKey
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (deleteAt) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (head, toArray) as NEArray
import Data.ByteArray (byteArrayToHex, hexToByteArray)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.String (Pattern(Pattern))
import Data.String (split, trim) as String
import Data.Traversable (for, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Debug (traceM)
import Effect (Effect)
import Effect.Exception (error)
import HydraSdk.Internal.Lib.Misc (concatPathSegments)
import HydraSdk.Internal.Process.HydraNode (HydraNodeStartupParams, HydraHeadPeer)
import HydraSdk.Internal.Types.Network (Network(Testnet))
import HydraSdk.Internal.Types.QueryLayer (QueryLayer(CardanoNode))
import Node.Buffer (toString) as Buffer
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (exists, mkdir, writeTextFile) as FSSync
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Nub, class Union) as Row
import Record (merge) as Record
import URI (Host(IPv4Address))
import URI.Host.IPv4Address (unsafeFromInts) as IPv4Address
import URI.Port (fromInt) as Port

type HydraClusterSpec =
  { workdir :: FilePath
  , nodeSocketPath :: FilePath
  , testnetMagic :: Int
  , protocolParametersPath :: FilePath
  , hydraNodeFirstPort :: Int
  , hydraNodeApiFirstPort :: Int
  , timeParams :: HydraClusterTimeParams
  }

type HydraClusterTimeParams =
  { contestPeriodSec :: Maybe Int
  , depositPeriodSec :: Maybe Int
  , unsyncedPeriodSec :: Maybe Int
  }

defaultHydraClusterTimeParamsForCardanoTestnet :: HydraClusterTimeParams
defaultHydraClusterTimeParamsForCardanoTestnet =
  { contestPeriodSec: Just 5
  , depositPeriodSec: Just 20
  , unsyncedPeriodSec: Nothing
  }

type HydraPeerSpec (peerExtra :: Row Type) =
  { cardanoSk :: PrivateKey
  , extra :: Record peerExtra
  }

genHydraNodeConfigurations
  :: forall (peerExtra :: Row Type)
   . Row.Union peerExtra (HydraHeadPeer ()) (HydraHeadPeer peerExtra)
  => Row.Nub (HydraHeadPeer peerExtra) (HydraHeadPeer peerExtra)
  => HydraClusterSpec
  -> NonEmptyArray (HydraPeerSpec peerExtra)
  -> Effect (Array (HydraNodeStartupParams peerExtra))
genHydraNodeConfigurations clusterConfig peerSpecs = do
  peers <- createWorkdirsAndStoreKeys
  hydraScripts <-
    publishHydraScripts clusterConfig.testnetMagic clusterConfig.nodeSocketPath
      (ops.mkCardanoSk $ _.workdir $ NEArray.head peers)
  worker (NEArray.toArray peers) hydraScripts
  where
  ops =
    { mkHydraNode: \idx ->
        Port.fromInt (clusterConfig.hydraNodeFirstPort + idx) <#>
          { hostname: localhost
          , port: _
          }
    , mkHydraNodeApi: \idx ->
        Port.fromInt (clusterConfig.hydraNodeApiFirstPort + idx) <#>
          { hostname: localhost
          , port: _
          }
    , mkPersistDir: flip concatPathSegments "persist-dir"
    , mkHydra: flip concatPathSegments "hydra"
    , mkHydraSk: flip concatPathSegments "hydra.sk"
    , mkHydraVk: flip concatPathSegments "hydra.vk"
    , mkCardanoSk: flip concatPathSegments "cardano.sk"
    , mkCardanoVk: flip concatPathSegments "cardano.vk"
    }

  worker
    :: Array { spec :: HydraPeerSpec peerExtra, idx :: Int, workdir :: FilePath }
    -> Array TransactionHash
    -> Effect (Array (HydraNodeStartupParams peerExtra))
  worker peers hydraScripts =
    for peers \{ idx, workdir } -> do
      hydraNodeAddress <-
        liftMaybe
          ( error $ "genHydraNodeConfigurations: invalid hydraNodeAddress for peer " <>
              show idx
          )
          (ops.mkHydraNode idx)
      hydraNodeApiAddress <-
        liftMaybe
          ( error $ "genHydraNodeConfigurations: invalid hydraNodeApiAddress for peer " <>
              show idx
          )
          (ops.mkHydraNodeApi idx)
      (peerConfigs :: Array (Record (HydraHeadPeer peerExtra))) <-
        traverse
          ( \peer -> do
              peerHydraNodeAddress <-
                liftMaybe
                  ( error $ "genHydraNodeConfigurations: invalid hydraNodeAddress for peer " <>
                      show peer.idx
                  )
                  (ops.mkHydraNode peer.idx)
              pure $
                Record.merge
                  peer.spec.extra
                  { hydraNodeAddress: peerHydraNodeAddress
                  , hydraVerificationKey: ops.mkHydraVk peer.workdir
                  , cardanoVerificationKey: ops.mkCardanoVk peer.workdir
                  }
          )
          (unsafePartial fromJust $ Array.deleteAt idx peers) -- safe
      pure
        { nodeId: toStringAs decimal idx
        , hydraNodeAddress
        , hydraNodeAdvertisedAddress: Nothing
        , hydraNodeApiAddress
        , persistDir: ops.mkPersistDir workdir
        , hydraSigningKey: ops.mkHydraSk workdir
        , cardanoSigningKey: ops.mkCardanoSk workdir
        , queryLayer:
            CardanoNode
              { network: Testnet { magic: clusterConfig.testnetMagic }
              , nodeSocket: clusterConfig.nodeSocketPath
              }
        , pparams: clusterConfig.protocolParametersPath
        , hydraScripts
        , contestPeriodSec: clusterConfig.timeParams.contestPeriodSec
        , depositPeriodSec: clusterConfig.timeParams.depositPeriodSec
        , unsyncedPeriodSec: clusterConfig.timeParams.unsyncedPeriodSec
        , peers: peerConfigs
        , etcd:
            { logLevel: Nothing -- Just Etcd.Warn 
            , logOutputs: Nothing
            }
        }

  createWorkdirsAndStoreKeys
    :: Effect
         ( NonEmptyArray
             { spec :: HydraPeerSpec peerExtra
             , idx :: Int
             , workdir :: FilePath
             }
         )
  createWorkdirsAndStoreKeys =
    peerSpecs # traverseWithIndex \idx peer -> do
      let
        nodeId = toStringAs decimal idx
        workdir = clusterConfig.workdir `concatPathSegments` nodeId
        cardanoVk = PrivateKey.toPublicKey peer.cardanoSk
      mkdirIfNotExists workdir
      privatePaymentKeyToFile (ops.mkCardanoSk workdir)
        peer.cardanoSk
      publicPaymentKeyToFile (ops.mkCardanoVk workdir)
        cardanoVk
      genHydraKeys $ ops.mkHydra workdir
      pure { spec: peer, idx, workdir }

genHydraKeys :: FilePath -> Effect Unit
genHydraKeys fp =
  void $ execSync ("hydra-node gen-hydra-key --output-file " <> fp)
    defaultExecSyncOptions

publishHydraScripts :: Int -> FilePath -> FilePath -> Effect (Array TransactionHash)
publishHydraScripts testnetMagic nodeSocket cardanoSk = do
  publishScriptsOutput <- Buffer.toString UTF8 =<< execSync cmd defaultExecSyncOptions
  traceM $ "publishScripts output: " <> publishScriptsOutput
  let txHashes = String.split (Pattern ",") $ String.trim publishScriptsOutput
  liftMaybe (error $ "publishHydraScripts: Could not decode tx hashes") $
    traverse (decodeCbor <<< wrap <=< hexToByteArray) txHashes
  where
  cmd :: String
  cmd =
    "hydra-node publish-scripts --testnet-magic "
      <> show testnetMagic
      <> " --node-socket "
      <> nodeSocket
      <> " --cardano-signing-key "
      <> cardanoSk

-- Helpers

localhost :: Host
localhost = IPv4Address $ IPv4Address.unsafeFromInts 127 0 0 1

mkdirIfNotExists :: FilePath -> Effect Unit
mkdirIfNotExists dir = do
  dirExists <- FSSync.exists dir
  unless dirExists $ FSSync.mkdir dir

publicPaymentKeyToFile :: FilePath -> PublicKey -> Effect Unit
publicPaymentKeyToFile fp key =
  FSSync.writeTextFile UTF8 fp keyJson
  where
  keyJson :: String
  keyJson =
    stringifyAeson $ encodeAeson $
      { "type": "PaymentVerificationKeyShelley_ed25519"
      , description: "Payment Verification Key"
      , cborHex: keyToCbor key
      }

  keyToCbor :: PublicKey -> String
  keyToCbor =
    (magicPrefix <> _)
      <<< byteArrayToHex
      <<< unwrap
      <<< PublicKey.toRawBytes

privatePaymentKeyToFile :: FilePath -> PrivateKey -> Effect Unit
privatePaymentKeyToFile fp key =
  FSSync.writeTextFile UTF8 fp keyJson
  where
  keyJson :: String
  keyJson =
    stringifyAeson $ encodeAeson $
      { "type": "PaymentSigningKeyShelley_ed25519"
      , description: "Payment Signing Key"
      , cborHex: keyToCbor key
      }

  keyToCbor :: PrivateKey -> String
  keyToCbor =
    (magicPrefix <> _)
      <<< byteArrayToHex
      <<< unwrap
      <<< PrivateKey.toRawBytes

magicPrefix :: String
magicPrefix = "5820"
