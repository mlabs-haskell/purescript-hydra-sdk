module HydraSdk.Internal.Types.NodeApiMessage
  ( CommittedMessage
  , GreetingsMessage
  , HeadAbortedMessage
  , HeadClosedMessage
  , HeadContestedMessage
  , HeadInitMessage
  , HeadFinalizedMessage
  , HeadOpenMessage
  , HydraNodeApi_InMessage
      ( Greetings
      , PeerConnected
      , PeerDisconnected
      , PeerHandshakeFailure
      , HeadIsInitializing
      , Committed
      , HeadIsOpen
      , HeadIsClosed
      , HeadIsContested
      , ReadyToFanout
      , HeadIsAborted
      , HeadIsFinalized
      , TxValid
      , TxInvalid
      , SnapshotConfirmed
      , InvalidInput
      )
  , HydraNodeApi_OutMessage
      ( Init
      , Abort
      , NewTx
      , Close
      , Contest
      , Fanout
      )
  , InvalidInputMessage
  , NewTxMessage
  , PeerConnMessage
  , PeerHandshakeFailureMessage
  , ReadyToFanoutMessage
  , SnapshotConfirmedMessage
  , TxInvalidMessage
  , TxValidMessage
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
import Data.Show.Generic (genericShow)
import HydraSdk.Internal.Lib.Codec
  ( dateTimeCodec
  , ed25519KeyHashCodec
  , fixTaggedSumCodec
  , scriptHashCodec
  , sumGenericCodec
  )
import HydraSdk.Internal.Types.ArgonautJson (ArgonautJson, argonautJsonCodec)
import HydraSdk.Internal.Types.HeadStatus (HydraHeadStatus, headStatusCodec)
import HydraSdk.Internal.Types.Snapshot (HydraSnapshot, hydraSnapshotCodec)
import HydraSdk.Internal.Types.Tx (HydraTx, hydraTxCodec)
import HydraSdk.Internal.Types.UtxoMap (HydraUtxoMap, hydraUtxoMapCodec)

----------------------------------------------------------------------
-- Incoming messages

-- TODO: add missing variants: PostTxOnChainFailed, CommandFailed, IgnoredHeadInitializing
data HydraNodeApi_InMessage
  = Greetings GreetingsMessage
  | PeerConnected PeerConnMessage
  | PeerDisconnected PeerConnMessage
  | PeerHandshakeFailure PeerHandshakeFailureMessage
  | HeadIsInitializing HeadInitMessage
  | Committed CommittedMessage
  | HeadIsOpen HeadOpenMessage
  | HeadIsClosed HeadClosedMessage
  | HeadIsContested HeadContestedMessage
  | ReadyToFanout ReadyToFanoutMessage
  | HeadIsAborted HeadAbortedMessage
  | HeadIsFinalized HeadFinalizedMessage
  | TxValid TxValidMessage
  | TxInvalid TxInvalidMessage
  | SnapshotConfirmed SnapshotConfirmedMessage
  | InvalidInput InvalidInputMessage

derive instance Generic HydraNodeApi_InMessage _
derive instance Eq HydraNodeApi_InMessage

instance Show HydraNodeApi_InMessage where
  show = genericShow

hydraNodeApiInMessageCodec :: CA.JsonCodec HydraNodeApi_InMessage
hydraNodeApiInMessageCodec =
  fixTaggedSumCodec $ sumGenericCodec "HydraNodeApi_InMessage"
    ( CAV.variantMatch
        { "Greetings": Right greetingsMessageCodec
        , "PeerConnected": Right peerConnMessageCodec
        , "PeerDisconnected": Right peerConnMessageCodec
        , "PeerHandshakeFailure": Right peerHandshakeFailureMessageCodec
        , "HeadIsInitializing": Right headInitMessageCodec
        , "Committed": Right committedMessageCodec
        , "HeadIsOpen": Right headOpenMessageCodec
        , "HeadIsClosed": Right headClosedMessageCodec
        , "HeadIsContested": Right headContestedMessageCodec
        , "ReadyToFanout": Right readyToFanoutMessageCodec
        , "HeadIsAborted": Right headAbortedMessageCodec
        , "HeadIsFinalized": Right headFinalizedMessageCodec
        , "TxValid": Right txValidMessageCodec
        , "TxInvalid": Right txInvalidMessageCodec
        , "SnapshotConfirmed": Right snapshotConfirmedMessageCodec
        , "InvalidInput": Right invalidInputMessageCodec
        }
    )

----------------------------------------------------------------------
-- 0. Greetings

-- | A friendly welcome message which tells a client something about
-- | the node. Currently used for knowing what Party the server
-- | embodies. This message produced whenever the hydra-node starts
-- | and clients should take consequence of seeing this. For example,
-- | we can assume no peers connected when we see 'Greetings'.
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
    { me: CA.object "GreetingsMessage:me" $ CAR.record
        { vkey: ed25519KeyHashCodec
        }
    , headStatus: headStatusCodec
    , snapshotUtxo: CAR.optional hydraUtxoMapCodec
    , hydraHeadId: CAR.optional scriptHashCodec
    , timestamp: dateTimeCodec
    , hydraNodeVersion: CA.string
    }

----------------------------------------------------------------------
-- 1. PeerConnected / 2. PeerDisconnected

-- | A message indicating a change in the connection status
-- | of a Head peer. 
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
-- 3. PeerHandshakeFailure

-- | A peer has failed to negotiate a protocol.
-- TODO: remoteHost: there appears to be a discrepancy between API
-- docs and the actual implementation
type PeerHandshakeFailureMessage =
  { remoteHost :: ArgonautJson
  , ourVersion :: Int
  , theirVersions :: Array Int
  , seq :: Int
  , timestamp :: DateTime
  }

peerHandshakeFailureMessageCodec :: CA.JsonCodec PeerHandshakeFailureMessage
peerHandshakeFailureMessageCodec =
  CA.object "PeerHandshakeFailureMessage" $ CAR.record
    { remoteHost: argonautJsonCodec
    , ourVersion: CA.int
    , theirVersions: CA.array CA.int
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- 4. HeadIsInitializing

-- | An Init transaction has been observed onchain, with the given
-- | Head ID.
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
    , parties:
        CA.array $ CA.object "HeadInitMessage:parties" $ CAR.record
          { vkey: ed25519KeyHashCodec
          }
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- 5. Committed

-- | A Commit transaction from a Head participant has been observed
-- | onchain.
type CommittedMessage =
  { party :: { vkey :: Ed25519KeyHash }
  , utxo :: HydraUtxoMap
  , seq :: Int
  , timestamp :: DateTime
  }

committedMessageCodec :: CA.JsonCodec CommittedMessage
committedMessageCodec =
  CA.object "CommittedMessage" $ CAR.record
    { party:
        CA.object "CommittedMessage:party" $ CAR.record
          { vkey: ed25519KeyHashCodec
          }
    , utxo: hydraUtxoMapCodec
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- 6. HeadIsOpen

-- | All parties have committed, and a successful CollectCom transaction
-- | was observed onchain.
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
-- 7. HeadIsClosed

-- | A Close transaction has been observed onchain, the head is now
-- | closed and the contestation phase begins.
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
-- 8. HeadIsContested

-- | A Contest transaction has been observed onchain, meaning that
-- | the Head state has been successfully contested and the returned
-- | snapshot number is now the latest accepted snapshot. The
-- | contestation phase was extended to the specified deadline.
type HeadContestedMessage =
  { headId :: ScriptHash
  , snapshotNumber :: Int
  , contestationDeadline :: DateTime
  , seq :: Int
  , timestamp :: DateTime
  }

headContestedMessageCodec :: CA.JsonCodec HeadContestedMessage
headContestedMessageCodec =
  CA.object "HeadContestedMessage" $ CAR.record
    { headId: scriptHashCodec
    , snapshotNumber: CA.int
    , contestationDeadline: dateTimeCodec
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- 9. ReadyToFanout

-- | The contestation period has passed and the Head can now be
-- | finalized by a Fanout transaction.
type ReadyToFanoutMessage =
  { headId :: ScriptHash
  , seq :: Int
  , timestamp :: DateTime
  }

readyToFanoutMessageCodec :: CA.JsonCodec ReadyToFanoutMessage
readyToFanoutMessageCodec =
  CA.object "ReadyToFanoutMessage" $ CAR.record
    { headId: scriptHashCodec
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- 10. HeadIsAborted

-- | One of the participants did Abort the Head before all commits
-- | were done or collected.
type HeadAbortedMessage =
  { headId :: ScriptHash
  , utxo :: HydraUtxoMap
  , seq :: Int
  , timestamp :: DateTime
  }

headAbortedMessageCodec :: CA.JsonCodec HeadAbortedMessage
headAbortedMessageCodec =
  CA.object "HeadAbortedMessage" $ CAR.record
    { headId: scriptHashCodec
    , utxo: hydraUtxoMapCodec
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- 11. HeadIsFinalized

-- | The Head was already closed and the contestation period
-- | is now over.
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
-- 12. TxValid

-- | Observed a valid transaction inside the Head. Note that a node
-- | observes its own transactions and it may still happen that this
-- | transaction is not included in a snapshot.
type TxValidMessage =
  { headId :: ScriptHash
  , transaction :: HydraTx
  , seq :: Int
  , timestamp :: DateTime
  }

txValidMessageCodec :: CA.JsonCodec TxValidMessage
txValidMessageCodec =
  CA.object "TxValidMessage" $ CAR.record
    { headId: scriptHashCodec
    , transaction: hydraTxCodec
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- 13. TxInvalid

-- | Observed an invalid transaction inside the head. Either it is not
-- | yet valid (because some other transactions need to be seen first),
-- | or it is no longer valid (because of conflicting transactions
-- | observed in-between). The included validation error should give
-- | an indication why it was not applicable to the given UTxO
-- | (the local, seen ledger state).
type TxInvalidMessage =
  { headId :: ScriptHash
  , utxo :: HydraUtxoMap
  , transaction :: HydraTx
  , validationError :: { reason :: String }
  , seq :: Int
  , timestamp :: DateTime
  }

txInvalidMessageCodec :: CA.JsonCodec TxInvalidMessage
txInvalidMessageCodec =
  CA.object "TxInvalidMessage" $ CAR.record
    { headId: scriptHashCodec
    , utxo: hydraUtxoMapCodec
    , transaction: hydraTxCodec
    , validationError:
        CA.object "TxInvalidMessage:validationError" $ CAR.record
          { reason: CA.string
          }
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- 14. SnapshotConfirmed

-- | The given snapshot has been multi-signed by all Head participants
-- | and is now confirmed.
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
-- 15. InvalidInput

-- | Emitted by the server when it has failed to parse some client
-- | input. It returns the malformed input as well as some hint about
-- | what went wrong.
type InvalidInputMessage =
  { reason :: String
  , input :: String
  , seq :: Int
  , timestamp :: DateTime
  }

invalidInputMessageCodec :: CA.JsonCodec InvalidInputMessage
invalidInputMessageCodec =
  CA.object "InvalidInputMessage" $ CAR.record
    { reason: CA.string
    , input: CA.string
    , seq: CA.int
    , timestamp: dateTimeCodec
    }

----------------------------------------------------------------------
-- Outcoming messages

data HydraNodeApi_OutMessage
  = Init
  | Abort
  | NewTx NewTxMessage
  | Close
  | Contest
  | Fanout

derive instance Generic HydraNodeApi_OutMessage _
derive instance Eq HydraNodeApi_OutMessage

instance Show HydraNodeApi_OutMessage where
  show = genericShow

hydraNodeApiOutMessageCodec :: CA.JsonCodec HydraNodeApi_OutMessage
hydraNodeApiOutMessageCodec =
  fixTaggedSumCodec $ sumGenericCodec "HydraNodeApi_OutMessage"
    ( CAV.variantMatch
        { "Init": Left unit
        , "Abort": Left unit
        , "NewTx": Right newTxMessageCodec
        , "Close": Left unit
        , "Contest": Left unit
        , "Fanout": Left unit
        }
    )

type NewTxMessage =
  { transaction :: HydraTx
  }

newTxMessageCodec :: CA.JsonCodec NewTxMessage
newTxMessageCodec =
  CA.object "NewTxMessage" $ CAR.record
    { transaction: hydraTxCodec
    }
