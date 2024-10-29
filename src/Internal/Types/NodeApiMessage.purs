module HydraSdk.Internal.Types.NodeApiMessage
  ( CommittedMessage
  , GreetingsMessage
  , HeadClosedMessage
  , HeadInitMessage
  , HeadFinalizedMessage
  , HeadOpenMessage
  , HydraNodeApi_InMessage
      ( In_Greetings
      , In_PeerConnected
      , In_PeerDisconnected
      , In_HeadIsInitializing
      , In_Committed
      , In_HeadIsAborted
      , In_HeadIsOpen
      , In_SnapshotConfirmed
      , In_TxInvalid
      , In_HeadIsClosed
      , In_ReadyToFanout
      , In_HeadIsFinalized
      )
  , HydraNodeApi_OutMessage
      ( Out_Init
      , Out_Abort
      , Out_NewTx
      , Out_Close
      , Out_Contest
      , Out_Fanout
      )
  , NewTxMessage
  , PeerConnMessage
  , SnapshotConfirmedMessage
  , hydraNodeApiInMessageCodec
  , hydraNodeApiOutMessageCodec
  ) where

import Prelude

import Cardano.Types (Ed25519KeyHash, ScriptHash)
import Data.Codec.Argonaut (JsonCodec, array, int, object, string) as CA
import Data.Codec.Argonaut.Record (optional, record) as CAR
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.DateTime (DateTime)
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.Variant (inj, match) as Variant
import HydraSdk.Internal.Lib.Codec
  ( dateTimeCodec
  , ed25519KeyHashCodec
  , fixTaggedSumCodec
  , scriptHashCodec
  )
import HydraSdk.Internal.Types.HeadStatus (HydraHeadStatus, headStatusCodec)
import HydraSdk.Internal.Types.Snapshot (HydraSnapshot, hydraSnapshotCodec)
import HydraSdk.Internal.Types.Tx (HydraTx, hydraTxCodec)
import HydraSdk.Internal.Types.UtxoMap (HydraUtxoMap, hydraUtxoMapCodec)
import Type.Proxy (Proxy(Proxy))

----------------------------------------------------------------------
-- Incoming messages

data HydraNodeApi_InMessage
  = In_Greetings GreetingsMessage
  | In_PeerConnected PeerConnMessage
  | In_PeerDisconnected PeerConnMessage
  | In_HeadIsInitializing HeadInitMessage
  | In_Committed CommittedMessage
  | In_HeadIsAborted
  | In_HeadIsOpen HeadOpenMessage
  | In_SnapshotConfirmed SnapshotConfirmedMessage
  | In_TxInvalid
  | In_HeadIsClosed HeadClosedMessage
  | In_ReadyToFanout
  | In_HeadIsFinalized HeadFinalizedMessage

derive instance Generic HydraNodeApi_InMessage _

instance Show HydraNodeApi_InMessage where
  show = genericShow

hydraNodeApiInMessageCodec :: CA.JsonCodec HydraNodeApi_InMessage
hydraNodeApiInMessageCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "Greetings": Right greetingsMessageCodec
          , "PeerConnected": Right peerConnMessageCodec
          , "PeerDisconnected": Right peerConnMessageCodec
          , "HeadIsInitializing": Right headInitMessageCodec
          , "Committed": Right committedMessageCodec
          , "HeadIsAborted": Left unit
          , "HeadIsOpen": Right headOpenMessageCodec
          , "SnapshotConfirmed": Right snapshotConfirmedMessageCodec
          , "TxInvalid": Left unit
          , "HeadIsClosed": Right headClosedMessageCodec
          , "ReadyToFanout": Left unit
          , "HeadIsFinalized": Right headFinalizedMessageCodec
          }
      )
  where
  toVariant = case _ of
    In_Greetings rec ->
      Variant.inj (Proxy :: Proxy "Greetings") rec
    In_PeerConnected rec ->
      Variant.inj (Proxy :: Proxy "PeerConnected") rec
    In_PeerDisconnected rec ->
      Variant.inj (Proxy :: Proxy "PeerDisconnected") rec
    In_HeadIsInitializing rec ->
      Variant.inj (Proxy :: Proxy "HeadIsInitializing") rec
    In_Committed rec ->
      Variant.inj (Proxy :: Proxy "Committed") rec
    In_HeadIsAborted ->
      Variant.inj (Proxy :: Proxy "HeadIsAborted") unit
    In_HeadIsOpen rec ->
      Variant.inj (Proxy :: Proxy "HeadIsOpen") rec
    In_SnapshotConfirmed rec ->
      Variant.inj (Proxy :: Proxy "SnapshotConfirmed") rec
    In_TxInvalid ->
      Variant.inj (Proxy :: Proxy "TxInvalid") unit
    In_HeadIsClosed rec ->
      Variant.inj (Proxy :: Proxy "HeadIsClosed") rec
    In_ReadyToFanout ->
      Variant.inj (Proxy :: Proxy "ReadyToFanout") unit
    In_HeadIsFinalized rec ->
      Variant.inj (Proxy :: Proxy "HeadIsFinalized") rec

  fromVariant = Variant.match
    { "Greetings": In_Greetings
    , "PeerConnected": In_PeerConnected
    , "PeerDisconnected": In_PeerDisconnected
    , "HeadIsInitializing": In_HeadIsInitializing
    , "Committed": In_Committed
    , "HeadIsAborted": const In_HeadIsAborted
    , "HeadIsOpen": In_HeadIsOpen
    , "SnapshotConfirmed": In_SnapshotConfirmed
    , "TxInvalid": const In_TxInvalid
    , "HeadIsClosed": In_HeadIsClosed
    , "ReadyToFanout": const In_ReadyToFanout
    , "HeadIsFinalized": In_HeadIsFinalized
    }

----------------------------------------------------------------------
-- PeerConnected / PeerDisconnected
-- A message indicating a change in the connection status
-- of a Head peer. 

type PeerConnMessage =
  { peer :: String
  , seq :: Int
  , timestamp :: DateTime
  }

peerConnMessageCodec :: CA.JsonCodec PeerConnMessage
peerConnMessageCodec =
  CA.object "PeerConnMessage" $ CAR.record
    { peer: CA.string
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- Greetings
-- A friendly welcome message which tells a client something about
-- the node. Currently used for knowing what Party the server
-- embodies. This message produced whenever the hydra-node starts and
-- clients should take consequence of seeing this. For example, we can
-- assume no peers connected when we see 'Greetings'.

type GreetingsMessage =
  { me :: { vkey :: Ed25519KeyHash }
  , headStatus :: HydraHeadStatus
  , hydraHeadId :: Maybe ScriptHash
  , snapshotUtxo :: Maybe HydraUtxoMap
  , timestamp :: DateTime
  , hydraNodeVersion :: String
  }

greetingsMessageCodec :: CA.JsonCodec GreetingsMessage
greetingsMessageCodec =
  CA.object "GreetingsMessage" $ CAR.record
    { me: CA.object "GreetingsMessage:vkey" $ CAR.record
        { vkey: ed25519KeyHashCodec
        }
    , headStatus: headStatusCodec
    , snapshotUtxo: CAR.optional hydraUtxoMapCodec
    , hydraHeadId: CAR.optional scriptHashCodec
    , timestamp: dateTimeCodec
    , hydraNodeVersion: CA.string
    }

----------------------------------------------------------------------
-- HeadIsInitializing
-- An Init transaction has been observed onchain, with the given
-- Head ID.

type HeadInitMessage =
  { headId :: ScriptHash
  , parties :: Array { vkey :: Ed25519KeyHash }
  , seq :: Int
  , timestamp :: DateTime
  }

headInitMessageCodec :: CA.JsonCodec HeadInitMessage
headInitMessageCodec =
  CA.object "HeadInitMessage" $ CAR.record
    { headId: scriptHashCodec
    , parties: CA.array $ CA.object "HeadInitMessage:vkey" $ CAR.record
        { vkey: ed25519KeyHashCodec
        }
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- Committed
-- A Commit transaction from a Head participant has been observed
-- onchain.

type CommittedMessage =
  { party :: { vkey :: Ed25519KeyHash }
  , utxo :: HydraUtxoMap
  , seq :: Int
  , timestamp :: DateTime
  }

committedMessageCodec :: CA.JsonCodec CommittedMessage
committedMessageCodec =
  CA.object "CommittedMessage" $ CAR.record
    { party: CA.object "CommittedMessage:vkey" $ CAR.record
        { vkey: ed25519KeyHashCodec
        }
    , utxo: hydraUtxoMapCodec
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- HeadIsOpen
-- All parties have committed, and a successful CollectCom transaction
-- was observed onchain.

type HeadOpenMessage =
  { headId :: ScriptHash
  , utxo :: HydraUtxoMap
  , seq :: Int
  , timestamp :: DateTime
  }

headOpenMessageCodec :: CA.JsonCodec HeadOpenMessage
headOpenMessageCodec =
  CA.object "HeadOpenMessage" $ CAR.record
    { headId: scriptHashCodec
    , utxo: hydraUtxoMapCodec
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- HeadIsClosed
-- A Close transaction has been observed onchain, the head is now
-- closed and the contestation phase begins.

type HeadClosedMessage =
  { headId :: ScriptHash
  , snapshotNumber :: Int
  , contestationDeadline :: DateTime
  , seq :: Int
  , timestamp :: DateTime
  }

headClosedMessageCodec :: CA.JsonCodec HeadClosedMessage
headClosedMessageCodec =
  CA.object "HeadClosedMessage" $ CAR.record
    { headId: scriptHashCodec
    , snapshotNumber: CA.int
    , contestationDeadline: dateTimeCodec
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- HeadIsFinalized
-- The Head was already closed and the contestation period
-- is now over.

type HeadFinalizedMessage =
  { headId :: ScriptHash
  , utxo :: HydraUtxoMap
  , seq :: Int
  , timestamp :: DateTime
  }

headFinalizedMessageCodec :: CA.JsonCodec HeadFinalizedMessage
headFinalizedMessageCodec =
  CA.object "HeadFinalizedMessage" $ CAR.record
    { headId: scriptHashCodec
    , utxo: hydraUtxoMapCodec
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- SnapshotConfirmed
-- The given snapshot has been multi-signed by all Head participants
-- and is now confirmed.

type SnapshotConfirmedMessage =
  { headId :: ScriptHash
  , snapshot :: HydraSnapshot
  , seq :: Int
  , timestamp :: DateTime
  }

snapshotConfirmedMessageCodec :: CA.JsonCodec SnapshotConfirmedMessage
snapshotConfirmedMessageCodec =
  CA.object "SnapshotConfirmedMessage" $ CAR.record
    { headId: scriptHashCodec
    , snapshot: hydraSnapshotCodec
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- Outcoming messages

data HydraNodeApi_OutMessage
  = Out_Init
  | Out_Abort
  | Out_NewTx NewTxMessage
  | Out_Close
  | Out_Contest
  | Out_Fanout

hydraNodeApiOutMessageCodec :: CA.JsonCodec HydraNodeApi_OutMessage
hydraNodeApiOutMessageCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "Init": Left unit
          , "Abort": Left unit
          , "NewTx": Right newTxMessageCodec
          , "Close": Left unit
          , "Contest": Left unit
          , "Fanout": Left unit
          }
      )
  where
  toVariant = case _ of
    Out_Init ->
      Variant.inj (Proxy :: Proxy "Init") unit
    Out_Abort ->
      Variant.inj (Proxy :: Proxy "Abort") unit
    Out_NewTx rec ->
      Variant.inj (Proxy :: Proxy "NewTx") rec
    Out_Close ->
      Variant.inj (Proxy :: Proxy "Close") unit
    Out_Contest ->
      Variant.inj (Proxy :: Proxy "Contest") unit
    Out_Fanout ->
      Variant.inj (Proxy :: Proxy "Fanout") unit

  fromVariant = Variant.match
    { "Init": const Out_Init
    , "Abort": const Out_Abort
    , "NewTx": Out_NewTx
    , "Close": const Out_Close
    , "Contest": const Out_Contest
    , "Fanout": const Out_Fanout
    }

type NewTxMessage =
  { transaction :: HydraTx
  }

newTxMessageCodec :: CA.JsonCodec NewTxMessage
newTxMessageCodec =
  CA.object "NewTxMessage" $ CAR.record
    { transaction: hydraTxCodec
    }
