module HydraSdk.Internal.Types.UtxoMap
  ( HydraUtxoMap(HydraUtxoMap)
  , fromUtxoMap
  , hydraUtxoMapCodec
  , toUtxoMap
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Plutus.Types.CurrencySymbol (mkCurrencySymbol)
import Cardano.Plutus.Types.TokenName (mkTokenName)
import Cardano.Plutus.Types.Value (Value) as Plutus
import Cardano.Plutus.Types.Value (lovelaceValueOf, singleton, toCardano) as Plutus.Value
import Cardano.Types
  ( Address
  , CborBytes
  , DataHash
  , Language(PlutusV1, PlutusV2, PlutusV3)
  , OutputDatum(OutputDatum, OutputDatumHash)
  , PlutusScript(PlutusScript)
  , ScriptRef(NativeScriptRef, PlutusScriptRef)
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , Value(Value)
  )
import Cardano.Types.AssetName (unAssetName)
import Cardano.Types.BigNum (fromBigInt, toBigInt) as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.OutputDatum (outputDatumDataHash, outputDatumDatum)
import Contract.CborBytes (cborBytesToHex)
import Contract.PlutusData (PlutusData(Constr, Map, List, Integer, Bytes))
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArray)
import Contract.Utxos (UtxoMap)
import Control.Alt ((<|>))
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Control.Safely (foldM)
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , Json
  , JsonDecodeError(AtKey, TypeMismatch, UnexpectedValue)
  , decodeJson
  , encodeJson
  , fromNumber
  , fromObject
  , fromString
  , (.:)
  , (.:?)
  )
import Data.Array ((:))
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (bitraverse)
import Data.Codec.Argonaut (JsonCodec, decode, encode, json, object, prismaticCodec) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (Either, hush, note)
import Data.Generic.Rep (class Generic)
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (delete, fromFoldable, toUnfoldable) as Obj
import HydraSdk.Internal.Lib.Codec
  ( addressCodec
  , cborBytesCodec
  , dataHashCodec
  , fromCaJsonDecodeError
  , printOref
  , readOref
  )
import JS.BigInt (BigInt)
import JS.BigInt (fromNumber, toNumber) as BigInt

newtype HydraUtxoMap = HydraUtxoMap (Array (TransactionInput /\ TransactionOutput))

derive instance Generic HydraUtxoMap _
derive instance Newtype HydraUtxoMap _
derive instance Eq HydraUtxoMap
derive newtype instance Semigroup HydraUtxoMap
derive newtype instance Monoid HydraUtxoMap

instance Show HydraUtxoMap where
  show = genericShow

instance EncodeJson HydraUtxoMap where
  encodeJson =
    fromObject
      <<< Obj.fromFoldable
      <<< map (bimap printOref (CA.encode txOutCodec))
      <<< unwrap

instance DecodeJson HydraUtxoMap where
  decodeJson =
    (map Obj.toUnfoldable <<< decodeJson) >=>
      ( map wrap <<< traverse
          ( bitraverse (note (TypeMismatch "TransactionInput") <<< readOref)
              (lmap fromCaJsonDecodeError <<< CA.decode txOutCodec)
          )
      )

hydraUtxoMapCodec :: CA.JsonCodec HydraUtxoMap
hydraUtxoMapCodec =
  CA.prismaticCodec "HydraUtxoMap" (hush <<< decodeJson) encodeJson
    CA.json

fromUtxoMap :: UtxoMap -> HydraUtxoMap
fromUtxoMap = wrap <<< Map.toUnfoldable

toUtxoMap :: HydraUtxoMap -> UtxoMap
toUtxoMap = Map.fromFoldable <<< unwrap

--

txOutCodec :: CA.JsonCodec TransactionOutput
txOutCodec =
  CA.prismaticCodec "TransactionOutput" (Just <<< fromHydraTxOut) toHydraTxOut
    hydraTxOutCodec
  where
  fromHydraTxOut :: HydraTxOut -> TransactionOutput
  fromHydraTxOut rec = wrap
    { address: rec.address
    , amount: rec.value
    , datum:
        maybe (OutputDatumHash <$> rec.datumhash) (Just <<< OutputDatum)
          rec.inlineDatum
    , scriptRef:
        rec.referenceScript >>= \{ script: { cborHex: scriptCbor, "type": scriptLang } } ->
          case scriptLang of
            SimpleScript -> NativeScriptRef <$> decodeCbor scriptCbor
            -- TODO: Plutus version encoded in CBOR?
            _ -> PlutusScriptRef <$> decodeCbor scriptCbor
    }

  toHydraTxOut :: TransactionOutput -> HydraTxOut
  toHydraTxOut (TransactionOutput rec) =
    { address: rec.address
    , value: rec.amount
    , inlineDatum: outputDatumDatum =<< rec.datum
    , inlineDatumhash: (map hashPlutusData <<< outputDatumDatum) =<< rec.datum
    , datum: Nothing -- FIXME: should we resolve the datum?
    , datumhash: outputDatumDataHash =<< rec.datum
    , referenceScript:
        rec.scriptRef <#> \scriptRef ->
          { script:
              case scriptRef of
                NativeScriptRef nativeScript ->
                  { cborHex: encodeCbor nativeScript
                  , "type": SimpleScript
                  }
                PlutusScriptRef plutusScript@(PlutusScript (_ /\ scriptLang)) ->
                  { cborHex: encodeCbor plutusScript
                  , "type":
                      case scriptLang of
                        PlutusV1 -> PlutusScriptV1
                        PlutusV2 -> PlutusScriptV2
                        PlutusV3 -> PlutusScriptV3
                  }
          }
    }

--

-- TODO: add a hybrid codec for handling optional nullable fields upstream
-- See https://github.com/garyb/purescript-codec-argonaut/issues/54
type HydraTxOut =
  { address :: Address
  , value :: Value
  , inlineDatum :: Maybe PlutusData
  , inlineDatumhash :: Maybe DataHash
  , datum :: Maybe PlutusData
  , datumhash :: Maybe DataHash
  , referenceScript :: Maybe { script :: HydraReferenceScript }
  }

hydraTxOutCodec :: CA.JsonCodec HydraTxOut
hydraTxOutCodec =
  CA.object "HydraTxOut" $ CAR.record
    { address: addressCodec
    , value: valueCodec
    , inlineDatum: CA.maybe plutusDataCodec
    , inlineDatumhash: CA.maybe dataHashCodec
    , datum: CA.maybe plutusDataCodec
    , datumhash: CA.maybe dataHashCodec
    , referenceScript:
        CA.maybe $ CA.object "HydraTxOut:referenceScript" $ CAR.record
          { script: hydraRefScriptCodec
          }
    }

type HydraReferenceScript =
  { cborHex :: CborBytes
  , "type" :: HydraReferenceScriptType
  }

hydraRefScriptCodec :: CA.JsonCodec HydraReferenceScript
hydraRefScriptCodec =
  CA.object "HydraReferenceScript" $ CAR.record
    { cborHex: cborBytesCodec
    , "type": hydraRefScriptTypeCodec
    }

data HydraReferenceScriptType
  = SimpleScript
  | PlutusScriptV1
  | PlutusScriptV2
  | PlutusScriptV3

derive instance Generic HydraReferenceScriptType _

hydraRefScriptTypeCodec :: CA.JsonCodec HydraReferenceScriptType
hydraRefScriptTypeCodec =
  CAG.nullarySum "HydraReferenceScriptType"

--

valueCodec :: CA.JsonCodec Value
valueCodec =
  CA.prismaticCodec "Value" (hush <<< decodeValue) encodeValue
    CA.json

encodeValue :: Value -> Json
encodeValue (Value coin multiAsset) =
  fromObject $ Obj.delete mempty $
    Obj.fromFoldable (lovelace : nonAdaAssets)
  where
  lovelace :: String /\ Json
  lovelace = "lovelace" /\ encodeJson (BigInt.toNumber $ BigNum.toBigInt $ unwrap coin)

  nonAdaAssets :: Array (String /\ Json)
  nonAdaAssets =
    (Map.toUnfoldable :: _ -> Array _) (unwrap multiAsset) <#> \(cs /\ mp) ->
      cborBytesToHex (encodeCbor cs) /\
        ( fromObject $ Obj.fromFoldable $
            (Map.toUnfoldable :: _ -> Array _) mp <#> \(tn /\ quantity) ->
              byteArrayToHex (unAssetName tn) /\ encodeJson
                (BigInt.toNumber $ BigNum.toBigInt quantity)
        )

decodeValue :: Json -> Either JsonDecodeError Value
decodeValue json = do
  obj <- decodeJson json
  let
    lovelaceKey = "lovelace"

    decodeLovelace :: Either JsonDecodeError (Maybe BigInt)
    decodeLovelace =
      runMaybeT do
        lovelaceNum <- MaybeT $ obj .:? lovelaceKey
        lift $ BigInt.fromNumber lovelaceNum #
          note (AtKey lovelaceKey $ UnexpectedValue $ fromNumber lovelaceNum)

    decodeNonAdaAssets :: Either JsonDecodeError Plutus.Value
    decodeNonAdaAssets =
      foldM
        ( \acc (csStr /\ tnList) -> do
            cs <- note (TypeMismatch "CurrencySymbol") $ mkCurrencySymbol =<< hexToByteArray
              csStr
            tnObj <- Obj.toUnfoldable <$> decodeJson tnList
            foldM
              ( \acc' (tnStr /\ quantityNum) -> do
                  tn <- note (TypeMismatch "TokenName") $ mkTokenName =<< hexToByteArray tnStr
                  quantity <- BigInt.fromNumber quantityNum #
                    note (AtKey tnStr $ UnexpectedValue $ fromNumber quantityNum)
                  pure $ acc' <> Plutus.Value.singleton cs tn quantity
              )
              acc
              tnObj
        )
        mempty
        (Obj.toUnfoldable $ Obj.delete lovelaceKey obj)

  lovelace <- decodeLovelace
  nonAdaAssets <- decodeNonAdaAssets
  let plutusValue = nonAdaAssets <> maybe mempty Plutus.Value.lovelaceValueOf lovelace
  note (TypeMismatch "Cardano.Value") $ Plutus.Value.toCardano plutusValue

--

plutusDataCodec :: CA.JsonCodec PlutusData
plutusDataCodec =
  CA.prismaticCodec "PlutusData" (hush <<< decodePlutusData) encodePlutusData
    CA.json

encodePlutusData :: PlutusData -> Json
encodePlutusData = case _ of
  Constr constr fields ->
    encodeJson
      { constructor: BigInt.toNumber $ BigNum.toBigInt constr
      , fields: encodePlutusData <$> fields
      }
  Map kvs ->
    encodeJson
      { map:
          kvs <#> \(k /\ v) ->
            { k: encodePlutusData k, v: encodePlutusData v }
      }
  List xs ->
    encodeJson
      { list: encodePlutusData <$> xs
      }
  Integer bi ->
    encodeJson
      { int: BigInt.toNumber bi
      }
  Bytes ba ->
    encodeJson
      { bytes: byteArrayToHex ba
      }

decodePlutusData :: Json -> Either JsonDecodeError PlutusData
decodePlutusData json = do
  obj <- decodeJson json
  let
    decodeConstr = do
      let constrKey = "constructor"
      constrNum <- obj .: constrKey
      constr <- (BigNum.fromBigInt =<< BigInt.fromNumber constrNum) #
        note (AtKey constrKey $ UnexpectedValue $ fromNumber constrNum)
      fields <- obj .: "fields"
      Constr constr <$> traverse decodePlutusData fields

    decodeMap = do
      (map' :: Array _) <- obj .: "map"
      Map <$> for map' \entry -> do
        key <- decodePlutusData =<< entry .: "k"
        value <- decodePlutusData =<< entry .: "v"
        pure $ key /\ value

    decodeList = do
      list <- obj .: "list"
      List <$> traverse decodePlutusData list

    decodeInteger = do
      let key = "int"
      num <- obj .: key
      Integer <$> BigInt.fromNumber num #
        note (AtKey key $ UnexpectedValue $ fromNumber num)

    decodeBytes = do
      let key = "bytes"
      bytesHex <- obj .: key
      Bytes <$> hexToByteArray bytesHex #
        note (AtKey key $ UnexpectedValue $ fromString bytesHex)

  decodeConstr
    <|> decodeMap
    <|> decodeList
    <|> decodeInteger
    <|> decodeBytes
