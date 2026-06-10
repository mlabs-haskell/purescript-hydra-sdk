-- | Re-exports various Hydra domain-specific types
-- | (such as `HydraHeadStatus` and `HydraNodeApi_InMessage`),
-- | along with other utility types (e.g., `HostPort` and `Network`).
module HydraSdk.Types
  ( module ExportCommitRequest
  , module ExportHeadStatus
  , module ExportHostPort
  , module ExportHttpError
  , module ExportNetwork
  , module ExportNodeApiMessage
  , module ExportQueryLayer
  , module ExportSnapshot
  , module ExportSyncStatus
  , module ExportTx
  , module ExportUtxoMap
  ) where

import HydraSdk.Internal.Http.Error
  ( AffjaxError(AffjaxError)
  , HttpError(DecodeJsonError, HttpRequestError, HttpResponseError)
  ) as ExportHttpError

import HydraSdk.Internal.Types.CommitRequest
  ( HydraCommitRequest(SimpleCommitRequest, FullCommitRequest)
  , hydraFullCommitRequestCodec
  , mkFullCommitRequest
  , mkSimpleCommitRequest
  ) as ExportCommitRequest

import HydraSdk.Internal.Types.HeadStatus
  ( HydraHeadStatus
      ( HeadStatus_Unknown
      , HeadStatus_Idle
      , HeadStatus_Initializing
      , HeadStatus_Open
      , HeadStatus_Closed
      , HeadStatus_FanoutPossible
      , HeadStatus_Final
      )
  , headStatusCodec
  , isHeadClosed
  , printHeadStatus
  , readHeadStatus
  ) as ExportHeadStatus

import HydraSdk.Internal.Types.HostPort
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
  ) as ExportHostPort

import HydraSdk.Internal.Types.Network
  ( Network(Testnet, Mainnet)
  , networkCodec
  , networkToNetworkId
  ) as ExportNetwork

import HydraSdk.Internal.Types.NodeApiMessage as ExportNodeApiMessage

import HydraSdk.Internal.Types.QueryLayer
  ( QueryLayer(CardanoNode, Blockfrost)
  , queryLayerCodec
  ) as ExportQueryLayer

import HydraSdk.Internal.Types.Snapshot
  ( ConfirmedSnapshot(InitialSnapshot, ConfirmedSnapshot)
  , HydraSnapshot(HydraSnapshot)
  , confirmedSnapshotCodec
  , emptySnapshot
  , hydraSnapshotCodec
  ) as ExportSnapshot

import HydraSdk.Internal.Types.SyncStatus
  ( SyncStatus(InSync, CatchingUp)
  , syncStatusCodec
  ) as ExportSyncStatus

import HydraSdk.Internal.Types.Tx
  ( HydraTx
  , hydraTxCodec
  ) as ExportTx

import HydraSdk.Internal.Types.UtxoMap
  ( HydraUtxoMap(HydraUtxoMap)
  , fromUtxoMap
  , hydraUtxoMapCodec
  , toUtxoMap
  ) as ExportUtxoMap
