module HydraSdk.Example.Minimal.Contract.PrepareUtxo
  ( prepareUtxoContract
  ) where

import Prelude

import Cardano.Types (TransactionInput, Value)
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.Transaction (_body)
import Cardano.Types.TransactionBody (_outputs)
import Cardano.Types.Value (lovelaceValueOf) as Value
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.Transaction (awaitTxConfirmed, balanceTx, signTransaction, submit)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints (mustPayToPubKey) as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Wallet (ownPaymentPubKeyHashes)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (findIndex, head) as Array
import Data.Lens ((^.))
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Effect.Exception (error)

prepareUtxoContract :: Contract TransactionInput
prepareUtxoContract = do
  logInfo' $ "prepareUtxoContract: start"
  pkh <- liftedM "prepareUtxoContract: Could not get own public key hash"
    (Array.head <$> ownPaymentPubKeyHashes)
  let
    value :: Value
    value = Value.lovelaceValueOf $ BigNum.fromInt 5_000_000

    constraints :: TxConstraints
    constraints = Constraints.mustPayToPubKey pkh value

  unbalancedTx /\ usedUtxos <- mkUnbalancedTx mempty constraints
  balancedTx <- balanceTx unbalancedTx usedUtxos mempty
  balancedSignedTx <- signTransaction balancedTx
  txHash <- submit balancedSignedTx
  awaitTxConfirmed txHash

  let txOutputs = balancedSignedTx ^. _body <<< _outputs
  index <- liftMaybe (error "prepareUtxoContract: Could not find target output") $
    Array.findIndex (eq value <<< _.amount <<< unwrap) txOutputs
  pure $ wrap
    { transactionId: txHash
    , index: UInt.fromInt index
    }
