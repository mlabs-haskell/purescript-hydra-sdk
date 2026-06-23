module HydraSdk.Example.Minimal.Contract.L2
  ( decommit
  , placeArbitraryDatumL2
  ) where

import Prelude

import Cardano.Types (Credential(PubKeyHashCredential), Transaction, UtxoMap, _body, _outputs)
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
import Control.Monad.Reader.Class (local)
import Data.Array (filter, find, head) as Array
import Data.Either (Either(Right))
import Data.Lens ((%~))
import Data.Map (fromFoldable, lookup, toUnfoldable) as Map
import Data.Maybe (Maybe(Just), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSampleOne)

decommit :: UtxoMap -> Contract Transaction
decommit snapshotUtxos =
  withPatchedGetUtxoByOrefQuery snapshotUtxos do
    pkh <- liftedM "decommit: Could not get own public key hash"
      (Array.head <$> ownPaymentPubKeyHashes)
    utxo <-
      liftMaybe (error "decommit: Could not find utxo locked at own address") $
        Array.find
          ( eq (Just $ wrap $ PubKeyHashCredential $ unwrap pkh)
              <<< getPaymentCredential
              <<< _.address
              <<< unwrap
              <<< snd
          )
          (Map.toUnfoldable snapshotUtxos)
    let
      constraints :: TxConstraints
      constraints = Constraints.mustSpendPubKeyOutput (fst utxo)

      balancerConstraints :: BalancerConstraints
      balancerConstraints = BalancerConstraints.mustUseUtxosAtAddresses mempty

      lookups :: ScriptLookups
      lookups = Lookups.unspentOutputs $ Map.fromFoldable [ utxo ]

    unbalancedTx /\ usedUtxos <- mkUnbalancedTx lookups constraints
    balancedTx <- balanceTx (removeTxOutputsWithEmptyValues unbalancedTx) usedUtxos
      balancerConstraints
    balancedSignedTx <- signTransaction balancedTx
    pure balancedSignedTx

placeArbitraryDatumL2 :: UtxoMap -> Contract Transaction
placeArbitraryDatumL2 snapshotUtxos = withPatchedGetUtxoByOrefQuery snapshotUtxos do
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
  balancedTx <- balanceTx (removeTxOutputsWithEmptyValues unbalancedTx) usedUtxos
    balancerConstraints
  balancedSignedTx <- signTransaction balancedTx
  pure balancedSignedTx

removeTxOutputsWithEmptyValues :: Transaction -> Transaction
removeTxOutputsWithEmptyValues tx =
  tx # _body <<< _outputs %~
    Array.filter (notEq Value.empty <<< _.amount <<< unwrap)

withPatchedGetUtxoByOrefQuery :: forall (a :: Type). UtxoMap -> Contract a -> Contract a
withPatchedGetUtxoByOrefQuery snapshotUtxos =
  local \env -> env
    { provider = env.provider
        { getUtxoByOref = \oref ->
            maybe (env.provider.getUtxoByOref oref) (pure <<< Right <<< Just) $
              Map.lookup oref snapshotUtxos
        }
    }
