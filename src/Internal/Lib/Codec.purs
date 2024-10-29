module HydraSdk.Internal.Lib.Codec
  ( addressCodec
  , byteArrayCodec
  , caDecodeString
  , caEncodeString
  , dateTimeCodec
  , ed25519KeyHashCodec
  , fixTaggedSumCodec
  , fromCaJsonDecodeError
  , orefCodec
  , printOref
  , readOref
  , scriptHashCodec
  , txCodec
  ) where

import Prelude

import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Types
  ( Address
  , CborBytes(CborBytes)
  , Ed25519KeyHash
  , ScriptHash
  , Transaction
  , TransactionInput(TransactionInput)
  )
import Cardano.Types.Address (fromBech32, toBech32) as Address
import Contract.CborBytes (cborBytesToHex, hexToCborBytes)
import Data.Argonaut (Json)
import Data.Argonaut
  ( JsonDecodeError(TypeMismatch, UnexpectedValue, AtIndex, AtKey, Named, MissingValue)
  , caseJsonObject
  , fromObject
  , parseJson
  , stringify
  ) as A
import Data.Bifunctor (lmap)
import Data.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Data.Codec.Argonaut
  ( Codec(Codec)
  , JsonCodec
  , JsonDecodeError(TypeMismatch, UnexpectedValue, AtIndex, AtKey, Named, MissingValue)
  , decode
  , encode
  , prismaticCodec
  , string
  ) as CA
import Data.DateTime (DateTime)
import Data.Either (Either, hush)
import Data.Formatter.DateTime (formatDateTime, unformatDateTime)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (wrap)
import Data.Profunctor (wrapIso)
import Data.String (Pattern(Pattern))
import Data.String (split) as String
import Data.Tuple (Tuple)
import Data.UInt (fromString, toString) as UInt
import Foreign.Object (delete, fromHomogeneous, lookup, member, size, union) as Obj
import Partial.Unsafe (unsafePartial)

fixTaggedSumCodec :: forall a. CA.JsonCodec a -> CA.JsonCodec a
fixTaggedSumCodec (CA.Codec dec enc) = CA.Codec decFixed encFixed
  where
  decFixed :: Json -> Either CA.JsonDecodeError a
  decFixed json =
    dec
      ( json # A.caseJsonObject json \obj ->
          case Obj.lookup "tag" obj, Obj.size obj > one of
            Just tag, true ->
              A.fromObject $
                Obj.fromHomogeneous
                  { tag
                  , value: A.fromObject (Obj.delete "tag" obj)
                  }
            _, _ -> json
      )

  encFixed :: a -> Tuple Json a
  encFixed =
    enc >>> lmap \json ->
      json # A.caseJsonObject json \obj ->
        case Obj.member "tag" obj, Obj.lookup "value" obj of
          true, Just valueJson ->
            valueJson # A.caseJsonObject json \valueObj ->
              A.fromObject $ Obj.union valueObj $ Obj.delete "value" obj
          _, _ -> json

fromCaJsonDecodeError :: CA.JsonDecodeError -> A.JsonDecodeError
fromCaJsonDecodeError = case _ of
  CA.TypeMismatch type_ -> A.TypeMismatch type_
  CA.UnexpectedValue json -> A.UnexpectedValue json
  CA.AtIndex idx err -> A.AtIndex idx $ fromCaJsonDecodeError err
  CA.AtKey key err -> A.AtKey key $ fromCaJsonDecodeError err
  CA.Named name err -> A.Named name $ fromCaJsonDecodeError err
  CA.MissingValue -> A.MissingValue

caDecodeString :: forall a. CA.JsonCodec a -> String -> Either CA.JsonDecodeError a
caDecodeString codec jsonStr = do
  json <- lmap (const (CA.TypeMismatch "JSON")) $ A.parseJson jsonStr
  CA.decode codec json

caEncodeString :: forall a. CA.JsonCodec a -> a -> String
caEncodeString codec = A.stringify <<< CA.encode codec

asCborCodec :: forall a. AsCbor a => String -> CA.JsonCodec a
asCborCodec name =
  CA.prismaticCodec name decodeCbor encodeCbor
    cborBytesCodec

addressCodec :: CA.JsonCodec Address
addressCodec =
  CA.prismaticCodec "Address" Address.fromBech32 Address.toBech32
    CA.string

byteArrayCodec :: CA.JsonCodec ByteArray
byteArrayCodec =
  CA.prismaticCodec "ByteArray" hexToByteArray byteArrayToHex
    CA.string

cborBytesCodec :: CA.JsonCodec CborBytes
cborBytesCodec = wrapIso CborBytes byteArrayCodec

dateTimeCodec :: CA.JsonCodec DateTime
dateTimeCodec =
  CA.prismaticCodec
    "DateTime"
    (hush <<< unformatDateTime formatter)
    (unsafePartial fromJust <<< hush <<< formatDateTime formatter)
    CA.string
  where
  formatter :: String
  formatter = "YYYY-MM-DDTHH:mm:ssZ"

ed25519KeyHashCodec :: CA.JsonCodec Ed25519KeyHash
ed25519KeyHashCodec = asCborCodec "Ed25519KeyHash"

orefCodec :: CA.JsonCodec TransactionInput
orefCodec =
  CA.prismaticCodec "TransactionInput" readOref printOref
    CA.string

printOref :: TransactionInput -> String
printOref (TransactionInput rec) =
  cborBytesToHex (encodeCbor rec.transactionId) <> "#" <> UInt.toString rec.index

readOref :: String -> Maybe TransactionInput
readOref str =
  case String.split (Pattern "#") str of
    [ txHashStr, idx ]
      | Just transactionId <- decodeCbor =<< hexToCborBytes txHashStr
      , Just index <- UInt.fromString idx ->
          Just $ wrap { transactionId, index }
    _ ->
      Nothing

scriptHashCodec :: CA.JsonCodec ScriptHash
scriptHashCodec = asCborCodec "ScriptHash"

txCodec :: CA.JsonCodec Transaction
txCodec = asCborCodec "Transaction"
