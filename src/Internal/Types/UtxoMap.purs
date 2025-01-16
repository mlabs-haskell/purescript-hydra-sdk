module HydraSdk.Internal.Types.UtxoMap
  ( HydraUtxoMap(HydraUtxoMap)
  , decodePlutusData
  , fromUtxoMap
  , hydraUtxoMapCodec
  , toUtxoMap
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , decodeAeson
  , encodeAeson
  , (.:)
  , (.:?)
  )
import Aeson (fromObject) as Aeson
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
  , PlutusData(Constr, Map, List, Integer, Bytes)
  , PlutusScript(PlutusScript)
  , ScriptRef(NativeScriptRef, PlutusScriptRef)
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , UtxoMap
  , Value(Value)
  )
import Cardano.Types.AssetName (unAssetName)
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.OutputDatum (outputDatumDataHash, outputDatumDatum)
import Control.Alt ((<|>))
import Control.Safely (foldM)
import Data.Argonaut (JsonDecodeError(AtKey, TypeMismatch, UnexpectedValue), fromString)
import Data.Array ((:))
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (bitraverse)
import Data.ByteArray (byteArrayToHex, hexToByteArray)
import Data.Codec.Argonaut (JsonCodec, decode, encode, json, object, prismaticCodec) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Codec.Argonaut.Record (optional, record) as CAR
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
  , aesonCodec
  , cborBytesCodec
  , dataHashCodec
  , fromCaJsonDecodeError
  , printOref
  , readOref
  )
import HydraSdk.Internal.Lib.Misc (cborBytesToHex)

newtype HydraUtxoMap = HydraUtxoMap (Array (TransactionInput /\ TransactionOutput))

derive instance Generic HydraUtxoMap _
derive instance Newtype HydraUtxoMap _
derive instance Eq HydraUtxoMap
derive newtype instance Semigroup HydraUtxoMap
derive newtype instance Monoid HydraUtxoMap

instance Show HydraUtxoMap where
  show = genericShow

instance EncodeAeson HydraUtxoMap where
  encodeAeson =
    Aeson.fromObject
      <<< Obj.fromFoldable
      <<< map (bimap printOref (CA.encode txOutCodec))
      <<< unwrap

instance DecodeAeson HydraUtxoMap where
  decodeAeson =
    (map Obj.toUnfoldable <<< decodeAeson) >=>
      ( map wrap <<< traverse
          ( bitraverse (note (TypeMismatch "TransactionInput") <<< readOref)
              (lmap fromCaJsonDecodeError <<< CA.decode txOutCodec)
          )
      )

hydraUtxoMapCodec :: CA.JsonCodec HydraUtxoMap
hydraUtxoMapCodec = aesonCodec "HydraUtxoMap"

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
        maybe (OutputDatumHash <$> join rec.datumhash) (Just <<< OutputDatum)
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
    , inlineDatumhash: map hashPlutusData <<< outputDatumDatum <$> rec.datum
    , datum: Nothing -- FIXME: should we resolve the datum?
    , datumhash: outputDatumDataHash <$> rec.datum
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
  , inlineDatumhash :: Maybe (Maybe DataHash)
  , datum :: Maybe PlutusData
  , datumhash :: Maybe (Maybe DataHash)
  , referenceScript :: Maybe { script :: HydraReferenceScript }
  }

hydraTxOutCodec :: CA.JsonCodec HydraTxOut
hydraTxOutCodec =
  CA.object "HydraTxOut" $ CAR.record
    { address: addressCodec
    , value: valueCodec
    , inlineDatum: CA.maybe plutusDataCodec
    , inlineDatumhash: CAR.optional $ CA.maybe dataHashCodec
    , datum: CA.maybe plutusDataCodec
    , datumhash: CAR.optional $ CA.maybe dataHashCodec
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

encodeValue :: Value -> Aeson
encodeValue (Value coin multiAsset) =
  Aeson.fromObject $ Obj.delete mempty $
    Obj.fromFoldable (lovelace : nonAdaAssets)
  where
  lovelace :: String /\ Aeson
  lovelace = "lovelace" /\ encodeAeson coin

  nonAdaAssets :: Array (String /\ Aeson)
  nonAdaAssets =
    (Map.toUnfoldable :: _ -> Array _) (unwrap multiAsset) <#> \(cs /\ mp) ->
      cborBytesToHex (encodeCbor cs) /\
        ( Aeson.fromObject $ Obj.fromFoldable $
            (Map.toUnfoldable :: _ -> Array _) mp <#> \(tn /\ quantity) ->
              byteArrayToHex (unAssetName tn) /\ encodeAeson quantity
        )

decodeValue :: Aeson -> Either JsonDecodeError Value
decodeValue json = do
  obj <- decodeAeson json
  let
    lovelaceKey = "lovelace"

    decodeNonAdaAssets :: Either JsonDecodeError Plutus.Value
    decodeNonAdaAssets =
      foldM
        ( \acc (csStr /\ tnList) -> do
            cs <- note (TypeMismatch "CurrencySymbol") $ mkCurrencySymbol =<< hexToByteArray
              csStr
            tnObj <- Obj.toUnfoldable <$> decodeAeson tnList
            foldM
              ( \acc' (tnStr /\ quantity) -> do
                  tn <- note (TypeMismatch "TokenName") $ mkTokenName =<< hexToByteArray tnStr
                  pure $ acc' <> Plutus.Value.singleton cs tn quantity
              )
              acc
              tnObj
        )
        mempty
        (Obj.toUnfoldable $ Obj.delete lovelaceKey obj)

  lovelace <- obj .:? lovelaceKey
  nonAdaAssets <- decodeNonAdaAssets
  let plutusValue = nonAdaAssets <> maybe mempty Plutus.Value.lovelaceValueOf lovelace
  note (TypeMismatch "Cardano.Value") $ Plutus.Value.toCardano plutusValue

--

plutusDataCodec :: CA.JsonCodec PlutusData
plutusDataCodec =
  CA.prismaticCodec "PlutusData" (hush <<< decodePlutusData) encodePlutusData
    CA.json

encodePlutusData :: PlutusData -> Aeson
encodePlutusData = case _ of
  Constr constructor fields ->
    encodeAeson
      { constructor
      , fields: encodePlutusData <$> fields
      }
  Map kvs ->
    encodeAeson
      { map:
          kvs <#> \(k /\ v) ->
            { k: encodePlutusData k, v: encodePlutusData v }
      }
  List xs ->
    encodeAeson
      { list: encodePlutusData <$> xs
      }
  Integer int -> encodeAeson { int }
  Bytes bytes -> encodeAeson { bytes }

decodePlutusData :: Aeson -> Either JsonDecodeError PlutusData
decodePlutusData json = do
  obj <- decodeAeson json
  let
    decodeConstr = do
      -- NOTE: The "constructor" property MUST be decoded after the
      -- "fields" property. The "constructor" is a prototype property
      -- and requires special handling. In JavaScript, objects with
      -- prototypes may inherit it, which will cause Aeson to throw
      -- cryptic errors when encountered.
      -- To avoid this, ensure that "fields" is decoded first.
      fields <- obj .: "fields"
      constr <- obj .: "constructor"
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

    decodeInteger = Integer <$> obj .: "int"

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
