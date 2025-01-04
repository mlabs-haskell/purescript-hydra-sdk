module HydraSdk.Example.Minimal.Contract.L2
  ( placeArbitraryDatumL2
  ) where

import Prelude

import Cardano.Types (Credential(PubKeyHashCredential), Transaction, UtxoMap)
import Cardano.Types.Address (getPaymentCredential)
import Cardano.Types.Value (empty) as Value
import Contract.BalanceTxConstraints (BalancerConstraints)
import Contract.BalanceTxConstraints (mustUseUtxosAtAddresses) as BalancerConstraints
import Contract.Monad (Contract, liftedM)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs) as Lookups
import Contract.Transaction (balanceTx, signTransaction)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints (mustPayToPubKeyWithDatum, mustSpendPubKeyOutput) as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Wallet (ownPaymentPubKeyHashes)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (find, head) as Array
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Maybe (Maybe(Just))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSampleOne)

placeArbitraryDatumL2 :: UtxoMap -> Contract Transaction
placeArbitraryDatumL2 snapshotUtxos = do
  pkh <- liftedM "placeArbitraryDatumL2: Could not get own public key hash"
    (Array.head <$> ownPaymentPubKeyHashes)
  utxo <-
    liftMaybe (error "placeArbitraryDatumL2: Could not find utxo locked at own address") $
      Array.find
        ( eq (Just $ wrap $ PubKeyHashCredential $ unwrap pkh)
            <<< getPaymentCredential
            <<< _.address
            <<< unwrap
            <<< snd
        )
        (Map.toUnfoldable snapshotUtxos)
  datum <- liftEffect $ randomSampleOne arbitrary
  let
    constraints :: TxConstraints
    constraints =
      Constraints.mustSpendPubKeyOutput (fst utxo)
        <> Constraints.mustPayToPubKeyWithDatum pkh datum DatumInline Value.empty

    balancerConstraints :: BalancerConstraints
    balancerConstraints = BalancerConstraints.mustUseUtxosAtAddresses mempty

    lookups :: ScriptLookups
    lookups = Lookups.unspentOutputs $ Map.fromFoldable [ utxo ]

  unbalancedTx /\ usedUtxos <- mkUnbalancedTx lookups constraints
  balancedTx <- balanceTx unbalancedTx usedUtxos balancerConstraints
  balancedSignedTx <- signTransaction balancedTx
  pure balancedSignedTx
