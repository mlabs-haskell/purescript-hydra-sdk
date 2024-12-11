module HydraSdk.Example.Minimal.Contract.Collateral
  ( getCollateral
  ) where

import Prelude

import Cardano.Types (TransactionInput, TransactionOutput(TransactionOutput), Value)
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.Transaction (_body)
import Cardano.Types.TransactionBody (_outputs)
import Cardano.Types.Value (lovelaceValueOf) as Value
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.Transaction (awaitTxConfirmed, balanceTx, signTransaction, submit)
import Contract.TxConstraints (mustPayToPubKey) as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHashes)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (findMap, findIndex, head) as Array
import Data.Lens ((^.))
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Effect.Exception (error)

collateralValue :: Value
collateralValue = Value.lovelaceValueOf $ BigNum.fromInt 5_000_000

getCollateral :: Contract TransactionInput
getCollateral = queryCollateral >>= maybe prepareCollateral pure

queryCollateral :: Contract (Maybe TransactionInput)
queryCollateral =
  getWalletUtxos <#> maybe Nothing
    ( Map.toUnfoldable >>>
        Array.findMap
          ( \(outRef /\ TransactionOutput { amount, datum, scriptRef }) ->
              case datum, scriptRef of
                Nothing, Nothing | amount == collateralValue ->
                  Just outRef
                _, _ ->
                  Nothing
          )
    )

prepareCollateral :: Contract TransactionInput
prepareCollateral = do
  logInfo' $ "prepareCollateral: start"
  pkh <- liftedM "prepareCollateral: Could not get own public key hash"
    (Array.head <$> ownPaymentPubKeyHashes)
  unbalancedTx /\ usedUtxos <- mkUnbalancedTx mempty $ Constraints.mustPayToPubKey pkh
    collateralValue
  balancedTx <- balanceTx unbalancedTx usedUtxos mempty
  balancedSignedTx <- signTransaction balancedTx
  txHash <- submit balancedSignedTx
  awaitTxConfirmed txHash

  let txOutputs = balancedSignedTx ^. _body <<< _outputs
  index <- liftMaybe (error "prepareCollateral: Could not find target output") $
    Array.findIndex (eq collateralValue <<< _.amount <<< unwrap) txOutputs
  pure $ wrap
    { transactionId: txHash
    , index: UInt.fromInt index
    }
