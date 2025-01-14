module HydraSdk.Internal.Types.NodeApiMessage
  ( CommittedMessage
  , GreetingsMessage
  , HeadAbortedMessage
  , HeadClosedMessage
  , HeadContestedMessage
  , HeadInitMessage
  , HeadFinalizedMessage
  , HeadOpenMessage
  , HeadParameters
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
      , PostTxOnChainFailed
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
  , PostChainTx
      ( InitTx
      , AbortTx
      , CollectComTx
      , IncrementTx
      , DecrementTx
      , CloseTx
      , ContestTx
      , FanoutTx
      )
  , PostTxError
      ( NoSeedInput
      , InvalidSeed
      , InvalidHeadId
      , CannotFindOwnInitial
      , UnsupportedLegacyOutput
      , InvalidStateToPost
      , NotEnoughFuel
      , NoFuelUTXOFound
      , ScriptFailedInWallet
      , InternalWalletError
      , FailedToPostTx
      , PlutusValidationFailed
      , CommittedTooMuchADAForMainnet
      , FailedToDraftTxNotInitializing
      , FailedToConstructAbortTx
      , FailedToConstructCloseTx
      , FailedToConstructContestTx
      , FailedToConstructCollectTx
      , FailedToConstructDecrementTx
      , FailedToConstructFanoutTx
      )
  , PostTxOnchainFailedMessage
  , PeerHandshakeFailureMessage
  , ReadyToFanoutMessage
  , SeqTimestamp
  , SnapshotConfirmedMessage
  , TxInvalidMessage
  , TxValidMessage
  , hydraNodeApiInMessageCodec
  , hydraNodeApiOutMessageCodec
  , nextHeadStatus
  ) where

import Prelude

import Aeson (Aeson)
import Cardano.Types (Coin, PublicKey, TransactionHash)
import Data.Codec.Argonaut (JPropCodec, JsonCodec, array, int, json, object, string) as CA
import Data.Codec.Argonaut.Record (class RowListCodec, optional, record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)
import HydraSdk.Internal.Lib.Codec (coinCodec, dateTimeCodec, publicKeyCodec, txHashCodec)
import HydraSdk.Internal.Types.HeadStatus
  ( HydraHeadStatus
      ( HeadStatus_Initializing
      , HeadStatus_Open
      , HeadStatus_Closed
      , HeadStatus_FanoutPossible
      , HeadStatus_Final
      )
  , headStatusCodec
  )
import HydraSdk.Internal.Types.Snapshot
  ( ConfirmedSnapshot
  , HydraSnapshot
  , confirmedSnapshotCodec
  , hydraSnapshotCodec
  )
import HydraSdk.Internal.Types.Tx (HydraTx, hydraTxCodec)
import HydraSdk.Internal.Types.UtxoMap (HydraUtxoMap, hydraUtxoMapCodec)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Record (merge) as Record

----------------------------------------------------------------------
-- Incoming messages

-- | Represents incoming messages from the hydra-node API WebSocket server.
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
  | PostTxOnChainFailed PostTxOnchainFailedMessage

derive instance Generic HydraNodeApi_InMessage _
derive instance Eq HydraNodeApi_InMessage

instance Show HydraNodeApi_InMessage where
  show = genericShow

hydraNodeApiInMessageCodec :: CA.JsonCodec HydraNodeApi_InMessage
hydraNodeApiInMessageCodec =
  CAS.sumFlat "HydraNodeApi_InMessage"
    { "Greetings": greetingsMessageCodec
    , "PeerConnected": peerConnMessageCodec
    , "PeerDisconnected": peerConnMessageCodec
    , "PeerHandshakeFailure": peerHandshakeFailureMessageCodec
    , "HeadIsInitializing": headInitMessageCodec
    , "Committed": committedMessageCodec
    , "HeadIsOpen": headOpenMessageCodec
    , "HeadIsClosed": headClosedMessageCodec
    , "HeadIsContested": headContestedMessageCodec
    , "ReadyToFanout": readyToFanoutMessageCodec
    , "HeadIsAborted": headAbortedMessageCodec
    , "HeadIsFinalized": headFinalizedMessageCodec
    , "TxValid": txValidMessageCodec
    , "TxInvalid": txInvalidMessageCodec
    , "SnapshotConfirmed": snapshotConfirmedMessageCodec
    , "InvalidInput": invalidInputMessageCodec
    , "PostTxOnChainFailed": postTxOnchainFailedMessageCodec
    }

-- | Determines the new Head status based on the incoming hydra-node API message.
-- | Returns `Nothing` if the message does not imply a status change.
nextHeadStatus :: HydraNodeApi_InMessage -> Maybe HydraHeadStatus
nextHeadStatus =
  case _ of
    Greetings { headStatus } -> Just headStatus
    HeadIsInitializing _ -> Just HeadStatus_Initializing
    HeadIsOpen _ -> Just HeadStatus_Open
    HeadIsClosed _ -> Just HeadStatus_Closed
    ReadyToFanout _ -> Just HeadStatus_FanoutPossible
    HeadIsAborted _ -> Just HeadStatus_Final
    HeadIsFinalized _ -> Just HeadStatus_Final
    _ -> Nothing

type SeqTimestamp (r :: Row Type) = Record
  ( seq :: Maybe Int
  , timestamp :: Maybe DateTime
  | r
  )

recCodecUnion
  :: forall c0 c1 c' c r rl
   . Union c0 c1 c'
  => Nub c' c
  => RowToList c rl
  => CAR.RowListCodec rl c r
  => Record c0
  -> Record c1
  -> CA.JPropCodec (Record r)
recCodecUnion rec0 = CAR.record <<< Record.merge rec0

seqTimestampCodec =
  recCodecUnion
    { seq: CAR.optional CA.int
    , timestamp: CAR.optional dateTimeCodec
    }

----------------------------------------------------------------------
-- 0. Greetings

-- | A friendly welcome message which tells a client something about
-- | the node. Currently used for knowing what Party the server
-- | embodies. This message produced whenever the hydra-node starts
-- | and clients should take consequence of seeing this. For example,
-- | we can assume no peers connected when we see 'Greetings'.
type GreetingsMessage =
  { me :: { vkey :: PublicKey }
  , headStatus :: HydraHeadStatus
  , hydraHeadId :: Maybe String -- ScriptHash
  , snapshotUtxo :: Maybe HydraUtxoMap
  , timestamp :: Maybe DateTime
  , hydraNodeVersion :: String
  }

greetingsMessageCodec :: CA.JPropCodec GreetingsMessage
greetingsMessageCodec =
  CAR.record
    { me: CA.object "GreetingsMessage:me" $ CAR.record
        { vkey: publicKeyCodec
        }
    , headStatus: headStatusCodec
    , snapshotUtxo: CAR.optional hydraUtxoMapCodec
    , hydraHeadId: CAR.optional CA.string -- scriptHashCodec
    , timestamp: CAR.optional dateTimeCodec
    , hydraNodeVersion: CA.string
    }

----------------------------------------------------------------------
-- 1. PeerConnected / 2. PeerDisconnected

-- | A message indicating a change in the connection status
-- | of a Head peer. 
type PeerConnMessage = SeqTimestamp
  ( peer :: String
  )

peerConnMessageCodec :: CA.JPropCodec PeerConnMessage
peerConnMessageCodec =
  seqTimestampCodec
    { peer: CA.string
    }

----------------------------------------------------------------------
-- 3. PeerHandshakeFailure

-- | A peer has failed to negotiate a protocol.
-- TODO: remoteHost: there appears to be a discrepancy between API
-- docs and the actual implementation
type PeerHandshakeFailureMessage = SeqTimestamp
  ( remoteHost :: Aeson
  , ourVersion :: Int
  , theirVersions :: Array Int
  )

peerHandshakeFailureMessageCodec :: CA.JPropCodec PeerHandshakeFailureMessage
peerHandshakeFailureMessageCodec =
  seqTimestampCodec
    { remoteHost: CA.json
    , ourVersion: CA.int
    , theirVersions: CA.array CA.int
    }

----------------------------------------------------------------------
-- 4. HeadIsInitializing

-- | An Init transaction has been observed onchain, with the given
-- | Head ID.
type HeadInitMessage = SeqTimestamp
  ( headId :: String -- ScriptHash
  , parties :: Array { vkey :: PublicKey }
  )

headInitMessageCodec :: CA.JPropCodec HeadInitMessage
headInitMessageCodec =
  seqTimestampCodec
    { headId: CA.string -- scriptHashCodec
    , parties:
        CA.array $ CA.object "HeadInitMessage:parties" $ CAR.record
          { vkey: publicKeyCodec
          }
    }

----------------------------------------------------------------------
-- 5. Committed

-- | A Commit transaction from a Head participant has been observed
-- | onchain.
type CommittedMessage = SeqTimestamp
  ( party :: { vkey :: PublicKey }
  , utxo :: HydraUtxoMap
  )

committedMessageCodec :: CA.JPropCodec CommittedMessage
committedMessageCodec =
  seqTimestampCodec
    { party:
        CA.object "CommittedMessage:party" $ CAR.record
          { vkey: publicKeyCodec
          }
    , utxo: hydraUtxoMapCodec
    }

----------------------------------------------------------------------
-- 6. HeadIsOpen

-- | All parties have committed, and a successful CollectCom transaction
-- | was observed onchain.
type HeadOpenMessage = SeqTimestamp
  ( headId :: String -- ScriptHash
  , utxo :: HydraUtxoMap
  )

headOpenMessageCodec :: CA.JPropCodec HeadOpenMessage
headOpenMessageCodec =
  seqTimestampCodec
    { headId: CA.string -- scriptHashCodec
    , utxo: hydraUtxoMapCodec
    }

----------------------------------------------------------------------
-- 7. HeadIsClosed

-- | A Close transaction has been observed onchain, the head is now
-- | closed and the contestation phase begins.
type HeadClosedMessage = SeqTimestamp
  ( headId :: String -- ScriptHash
  , snapshotNumber :: Int
  , contestationDeadline :: DateTime
  )

headClosedMessageCodec :: CA.JPropCodec HeadClosedMessage
headClosedMessageCodec =
  seqTimestampCodec
    { headId: CA.string -- scriptHashCodec
    , snapshotNumber: CA.int
    , contestationDeadline: dateTimeCodec
    }

----------------------------------------------------------------------
-- 8. HeadIsContested

-- | A Contest transaction has been observed onchain, meaning that
-- | the Head state has been successfully contested and the returned
-- | snapshot number is now the latest accepted snapshot. The
-- | contestation phase was extended to the specified deadline.
type HeadContestedMessage = SeqTimestamp
  ( headId :: String -- ScriptHash
  , snapshotNumber :: Int
  , contestationDeadline :: DateTime
  )

headContestedMessageCodec :: CA.JPropCodec HeadContestedMessage
headContestedMessageCodec =
  seqTimestampCodec
    { headId: CA.string -- scriptHashCodec
    , snapshotNumber: CA.int
    , contestationDeadline: dateTimeCodec
    }

----------------------------------------------------------------------
-- 9. ReadyToFanout

-- | The contestation period has passed and the Head can now be
-- | finalized by a Fanout transaction.
type ReadyToFanoutMessage = SeqTimestamp
  ( headId :: String -- ScriptHash
  )

readyToFanoutMessageCodec :: CA.JPropCodec ReadyToFanoutMessage
readyToFanoutMessageCodec =
  seqTimestampCodec
    { headId: CA.string -- scriptHashCodec
    }

----------------------------------------------------------------------
-- 10. HeadIsAborted

-- | One of the participants did Abort the Head before all commits
-- | were done or collected.
type HeadAbortedMessage = SeqTimestamp
  ( headId :: String -- ScriptHash
  , utxo :: HydraUtxoMap
  )

headAbortedMessageCodec :: CA.JPropCodec HeadAbortedMessage
headAbortedMessageCodec =
  seqTimestampCodec
    { headId: CA.string -- scriptHashCodec
    , utxo: hydraUtxoMapCodec
    }

----------------------------------------------------------------------
-- 11. HeadIsFinalized

-- | The Head was already closed and the contestation period
-- | is now over.
type HeadFinalizedMessage = SeqTimestamp
  ( headId :: String -- ScriptHash
  , utxo :: HydraUtxoMap
  )

headFinalizedMessageCodec :: CA.JPropCodec HeadFinalizedMessage
headFinalizedMessageCodec =
  seqTimestampCodec
    { headId: CA.string -- scriptHashCodec
    , utxo: hydraUtxoMapCodec
    }

----------------------------------------------------------------------
-- 12. TxValid

-- | Observed a valid transaction inside the Head. Note that a node
-- | observes its own transactions and it may still happen that this
-- | transaction is not included in a snapshot.
type TxValidMessage = SeqTimestamp
  ( headId :: String -- ScriptHash
  , transaction :: HydraTx
  )

txValidMessageCodec :: CA.JPropCodec TxValidMessage
txValidMessageCodec =
  seqTimestampCodec
    { headId: CA.string -- scriptHashCodec
    , transaction: hydraTxCodec
    }

----------------------------------------------------------------------
-- 13. TxInvalid

-- | Observed an invalid transaction inside the head. Either it is not
-- | yet valid (because some other transactions need to be seen first),
-- | or it is no longer valid (because of conflicting transactions
-- | observed in-between). The included validation error should give
-- | an indication why it was not applicable to the given UTxO
-- | (the local, seen ledger state).
type TxInvalidMessage = SeqTimestamp
  ( headId :: String -- ScriptHash
  , utxo :: HydraUtxoMap
  , transaction :: HydraTx
  , validationError :: { reason :: String }
  )

txInvalidMessageCodec :: CA.JPropCodec TxInvalidMessage
txInvalidMessageCodec =
  seqTimestampCodec
    { headId: CA.string -- scriptHashCodec
    , utxo: hydraUtxoMapCodec
    , transaction: hydraTxCodec
    , validationError:
        CA.object "TxInvalidMessage:validationError" $ CAR.record
          { reason: CA.string
          }
    }

----------------------------------------------------------------------
-- 14. SnapshotConfirmed

-- | The given snapshot has been multi-signed by all Head participants
-- | and is now confirmed.
type SnapshotConfirmedMessage = SeqTimestamp
  ( headId :: String -- ScriptHash
  , snapshot :: HydraSnapshot
  )

snapshotConfirmedMessageCodec :: CA.JPropCodec SnapshotConfirmedMessage
snapshotConfirmedMessageCodec =
  seqTimestampCodec
    { headId: CA.string -- scriptHashCodec
    , snapshot: hydraSnapshotCodec
    }

----------------------------------------------------------------------
-- 15. InvalidInput

-- | Emitted by the server when it has failed to parse some client
-- | input. It returns the malformed input as well as some hint about
-- | what went wrong.
type InvalidInputMessage = SeqTimestamp
  ( reason :: String
  , input :: String
  )

invalidInputMessageCodec :: CA.JPropCodec InvalidInputMessage
invalidInputMessageCodec =
  seqTimestampCodec
    { reason: CA.string
    , input: CA.string
    }

----------------------------------------------------------------------
-- 16. PostTxOnchainFailedMessage

-- | Something wrong happened when trying to post a transaction
-- | on-chain. Provides information about what kind of transaction was
-- | tentatively posted, and the reason for failure.
type PostTxOnchainFailedMessage = SeqTimestamp
  ( postChainTx :: PostChainTx
  , postTxError :: PostTxError
  )

postTxOnchainFailedMessageCodec :: CA.JPropCodec PostTxOnchainFailedMessage
postTxOnchainFailedMessageCodec =
  seqTimestampCodec
    { postChainTx: postChainTxCodec
    , postTxError: postTxErrorCodec
    }

type HeadParameters =
  { contestationPeriod :: Int
  , parties :: Array { vkey :: PublicKey }
  }

headParametersCodec :: CA.JsonCodec HeadParameters
headParametersCodec =
  CA.object "HeadParameters" $ CAR.record
    { contestationPeriod: CA.int
    , parties:
        CA.array $ CA.object "HeadParameters:parties" $ CAR.record
          { vkey: publicKeyCodec
          }
    }

-- | Outgoing on-chain transaction for the Head protocol.
-- | Reference: https://github.com/cardano-scaling/hydra/blob/1ffe7c6b505e3f38b5546ae5e5b97de26bc70425/hydra-node/src/Hydra/Chain.hs#L51-L75
data PostChainTx
  = InitTx
      { participants :: Array String
      , headParameters :: HeadParameters
      }
  | AbortTx
      { utxo :: HydraUtxoMap
      , headSeed :: String
      }
  | CollectComTx
      { utxo :: HydraUtxoMap
      , headId :: String
      , headParameters :: HeadParameters
      }
  | IncrementTx
      { headId :: String
      , headParameters :: HeadParameters
      , incrementingSnapshot :: ConfirmedSnapshot
      , depositTxId :: TransactionHash
      }
  | DecrementTx
      { headId :: String
      , headParameters :: HeadParameters
      , decrementingSnapshot :: ConfirmedSnapshot
      }
  | CloseTx
      { headId :: String
      , headParameters :: HeadParameters
      , closingSnapshot :: ConfirmedSnapshot
      , openVersion :: Int
      }
  | ContestTx
      { headId :: String
      , headParameters :: HeadParameters
      , contestingSnapshot :: ConfirmedSnapshot
      , openVersion :: Int
      }
  | FanoutTx
      { utxo :: HydraUtxoMap
      , utxoToDecommit :: HydraUtxoMap
      , headSeed :: String
      , contestationDeadline :: DateTime
      }

derive instance Generic PostChainTx _
derive instance Eq PostChainTx

instance Show PostChainTx where
  show = genericShow

postChainTxCodec :: CA.JsonCodec PostChainTx
postChainTxCodec =
  CAS.sumFlat "PostChainTx"
    { "InitTx":
        CAR.record
          { participants: CA.array CA.string
          , headParameters: headParametersCodec
          }
    , "AbortTx":
        CAR.record
          { utxo: hydraUtxoMapCodec
          , headSeed: CA.string
          }
    , "CollectComTx":
        CAR.record
          { utxo: hydraUtxoMapCodec
          , headId: CA.string
          , headParameters: headParametersCodec
          }
    , "IncrementTx":
        CAR.record
          { headId: CA.string
          , headParameters: headParametersCodec
          , incrementingSnapshot: confirmedSnapshotCodec
          , depositTxId: txHashCodec
          }
    , "DecrementTx":
        CAR.record
          { headId: CA.string
          , headParameters: headParametersCodec
          , decrementingSnapshot: confirmedSnapshotCodec
          }
    , "CloseTx":
        CAR.record
          { headId: CA.string
          , headParameters: headParametersCodec
          , closingSnapshot: confirmedSnapshotCodec
          , openVersion: CA.int
          }
    , "ContestTx":
        CAR.record
          { headId: CA.string
          , headParameters: headParametersCodec
          , contestingSnapshot: confirmedSnapshotCodec
          , openVersion: CA.int
          }
    , "FanoutTx":
        CAR.record
          { utxo: hydraUtxoMapCodec
          , utxoToDecommit: hydraUtxoMapCodec
          , headSeed: CA.string
          , contestationDeadline: dateTimeCodec
          }
    }

-- | Possible transaction submission errors.
-- |
-- | `NoSeedInput`: Initialising a new Head failed because the DirectChain
-- | component was unable to find a "seed" UTxO to consume. This can happen if
-- | no UTxO has been assigned to the internal wallet's address for this
-- | purpose, or if the component is still catching up with the chain. This
-- | error is usually transient and clients should retry to post
-- | the transaction.
-- |
-- | `InvalidSeed`: Raised if the user fails to submit an abort or fanout tx
-- | using the wrong seed.
-- |
-- | `InvalidHeadId`: Raised if the user fails to submit a commit tx using
-- | the wrong headId.
-- |
-- | `CannotFindOwnInitial`: The DirectChain was unable to find the output
-- | paying to Initial script corresponding to this node's Party, with the
-- | relevant Participation Token.
-- |
-- | `UnsupportedLegacyOutput`: The UTxO provided to commit is locked by
-- | a (legacy) Byron address, which is not supported. 
-- |
-- | `InvalidStateToPost`: Attempted to post a transaction that's invalid given
-- | current protocol's state. This is definitely a BUG.
-- |
-- | `NotEnoughFuel`: Raised if the internal wallet could not find a fuel output
-- | with enough Lovelace to balance a transaction. 
-- |
-- | `NoFuelUTXOFound`: Raised if the internal wallet could not find a fuel
-- | output.
-- |
-- | `ScriptFailedInWallet`: Script execution failed when finalizing
-- | a transaction in the wallet. The redeemer pointer should give an indication
-- | which script failed while the reason is a dump of the error.
-- |
-- | `InternalWalletError`:  Some input in a transaction cannot be resolved to
-- | a proper UTxO. This can happen either when some input is double-spent or
-- | the DirectChain component has not yet caught up with the chain.
-- |
-- | `FailedToPostTx`: A generic error case. Some transaction that wasn't
-- | expected to fail still failed... somehow.
-- |
-- | `PlutusValidationFailed`: An internal transaction created by the Hydra node
-- | failed with Plutus errors.
-- |
-- | `CommittedTooMuchADAForMainnet`: Raised if the user tries to commit more
-- | than 100 ADA while on the mainnet network.
-- |
-- | `FailedToDraftTxNotInitializing`: Raised if the user tried to draft
-- | a commit tx while Head is not in Initializing state.
-- |
-- | Reference: https://github.com/cardano-scaling/hydra/blob/1ffe7c6b505e3f38b5546ae5e5b97de26bc70425/hydra-node/src/Hydra/Chain.hs#L135-L171
data PostTxError
  = NoSeedInput
  | InvalidSeed { headSeed :: String }
  | InvalidHeadId { headId :: String }
  | CannotFindOwnInitial { knownUTxO :: HydraUtxoMap }
  | UnsupportedLegacyOutput { byronAddress :: String }
  | InvalidStateToPost { chainState :: Aeson, txTried :: Aeson }
  | NotEnoughFuel
  | NoFuelUTXOFound
  | ScriptFailedInWallet { redeemerPtr :: String, failureReason :: String }
  | InternalWalletError { tx :: HydraTx, reason :: String, headUTxO :: HydraUtxoMap }
  | FailedToPostTx { failureReason :: String }
  | PlutusValidationFailed { plutusFailure :: String, plutusDebugInfo :: String }
  | CommittedTooMuchADAForMainnet
      { userCommittedLovelace :: Coin
      , mainnetLimitLovelace :: Coin
      }
  | FailedToDraftTxNotInitializing
  | FailedToConstructAbortTx
  | FailedToConstructCloseTx
  | FailedToConstructContestTx
  | FailedToConstructCollectTx
  | FailedToConstructDecrementTx
  | FailedToConstructFanoutTx

derive instance Generic PostTxError _
derive instance Eq PostTxError

instance Show PostTxError where
  show = genericShow

postTxErrorCodec :: CA.JsonCodec PostTxError
postTxErrorCodec =
  CAS.sumFlat "PostTxError"
    { "NoSeedInput": unit
    , "InvalidSeed": CAR.record { headSeed: CA.string }
    , "InvalidHeadId": CAR.record { headId: CA.string }
    , "CannotFindOwnInitial": CAR.record { knownUTxO: hydraUtxoMapCodec }
    , "UnsupportedLegacyOutput": CAR.record { byronAddress: CA.string }
    , "InvalidStateToPost": CAR.record { chainState: CA.json, txTried: CA.json }
    , "NotEnoughFuel": unit
    , "NoFuelUTXOFound": unit
    , "ScriptFailedInWallet": CAR.record { redeemerPtr: CA.string, failureReason: CA.string }
    , "InternalWalletError":
        CAR.record
          { tx: hydraTxCodec
          , reason: CA.string
          , headUTxO: hydraUtxoMapCodec
          }
    , "FailedToPostTx": CAR.record { failureReason: CA.string }
    , "PlutusValidationFailed":
        CAR.record
          { plutusFailure: CA.string
          , plutusDebugInfo: CA.string
          }
    , "CommittedTooMuchADAForMainnet":
        CAR.record
          { userCommittedLovelace: coinCodec
          , mainnetLimitLovelace: coinCodec
          }
    , "FailedToDraftTxNotInitializing": unit
    , "FailedToConstructAbortTx": unit
    , "FailedToConstructCloseTx": unit
    , "FailedToConstructContestTx": unit
    , "FailedToConstructCollectTx": unit
    , "FailedToConstructDecrementTx": unit
    , "FailedToConstructFanoutTx": unit
    }

----------------------------------------------------------------------
-- Outcoming messages

-- | Represents messages that can be sent to the hydra-node API
-- | WebSocket server.
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
  CAS.sumFlat "HydraNodeApi_OutMessage"
    { "Init": unit
    , "Abort": unit
    , "NewTx": newTxMessageCodec
    , "Close": unit
    , "Contest": unit
    , "Fanout": unit
    }

type NewTxMessage =
  { transaction :: HydraTx
  }

newTxMessageCodec :: CA.JPropCodec NewTxMessage
newTxMessageCodec =
  CAR.record
    { transaction: hydraTxCodec
    }
