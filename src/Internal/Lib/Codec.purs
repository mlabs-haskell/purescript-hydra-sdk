-- | This module provides bidirectional JSON codecs for commonly used
-- | types in the SDK along with useful helper functions.
module HydraSdk.Internal.Lib.Codec
  ( addressCodec
  , byteArrayCodec
  , caDecodeFile
  , caDecodeString
  , caEncodeString
  , cborBytesCodec
  , dataHashCodec
  , dateTimeCodec
  , ed25519KeyHashCodec
  , fixTaggedSumCodec
  , fromCaJsonDecodeError
  , logLevelCodec
  , orefCodec
  , printOref
  , publicKeyCodec
  , rawBytesCodec
  , readOref
  , scriptHashCodec
  , txCodec
  , txHashCodec
  ) where

import Prelude

import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Types
  ( Address
  , CborBytes(CborBytes)
  , DataHash
  , Ed25519KeyHash
  , PublicKey
  , RawBytes(RawBytes)
  , ScriptHash
  , Transaction
  , TransactionHash
  , TransactionInput(TransactionInput)
  )
import Cardano.Types.Address (fromBech32, toBech32) as Address
import Cardano.Types.PublicKey (fromRawBytes, toRawBytes) as PublicKey
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
import Data.Log.Level (LogLevel(Trace, Debug, Info, Warn, Error))
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (wrap)
import Data.Profunctor (wrapIso)
import Data.String (Pattern(Pattern))
import Data.String (split, stripSuffix, take) as String
import Data.Tuple (Tuple)
import Data.UInt (fromString, toString) as UInt
import Effect (Effect)
import Foreign.Object (delete, fromHomogeneous, lookup, member, size, union) as Obj
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.FS.Sync (readTextFile) as FSSync
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)

-- | Builds a flat sum type codec from the provided sum type codec by
-- | embedding the `"value"` contents during encoding and combining
-- | them under `"value"` during decoding to make it compatible
-- | with the Haskell Aeson format.
-- FIXME: handle all edge cases
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

-- | Converts `JsonDecodeError` defined in `Data.Codec.Argonaut` to
-- | `JsonDecodeError` defined in `Data.Argonaut.Decode.Error`.
-- | Both types are identical and can be converted without
-- | any loss of information.
-- |
-- | Relevant issue:
-- | https://github.com/purescript-contrib/purescript-argonaut-codecs/issues/83
fromCaJsonDecodeError :: CA.JsonDecodeError -> A.JsonDecodeError
fromCaJsonDecodeError = case _ of
  CA.TypeMismatch type_ -> A.TypeMismatch type_
  CA.UnexpectedValue json -> A.UnexpectedValue json
  CA.AtIndex idx err -> A.AtIndex idx $ fromCaJsonDecodeError err
  CA.AtKey key err -> A.AtKey key $ fromCaJsonDecodeError err
  CA.Named name err -> A.Named name $ fromCaJsonDecodeError err
  CA.MissingValue -> A.MissingValue

-- | Attempts to decode the given JSON file using the specified codec.
caDecodeFile :: forall a. CA.JsonCodec a -> FilePath -> Effect (Either CA.JsonDecodeError a)
caDecodeFile codec =
  map (caDecodeString codec)
    <<< FSSync.readTextFile Encoding.UTF8

-- | Attempts to parse a string as JSON and then decode it using
-- | the specified codec.
caDecodeString :: forall a. CA.JsonCodec a -> String -> Either CA.JsonDecodeError a
caDecodeString codec jsonStr = do
  json <- lmap (const (CA.TypeMismatch "JSON")) $ A.parseJson jsonStr
  CA.decode codec json

-- | Converts the provided value into a JSON string using
-- | the specified codec. 
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

dataHashCodec :: CA.JsonCodec DataHash
dataHashCodec = asCborCodec "DataHash"

-- | Bidirectional JSON codec for `DateTime`. Attempts to handle the ambiguity
-- | in timestamps returned by hydra-node, where some may include nanoseconds
-- | while others omit fractional seconds entirely, etc.
dateTimeCodec :: CA.JsonCodec DateTime
dateTimeCodec =
  CA.prismaticCodec
    "DateTime"
    ( \str -> do
        strFixed <- String.stripSuffix (Pattern "Z") str <#> \strNoTimezone ->
          case String.split (Pattern ".") strNoTimezone of
            [ a, b ] ->
              -- truncate to milliseconds
              a <> "." <> String.take 3 (b <> "00") <> "Z"
            _ ->
              strNoTimezone <> ".000Z"
        hush $ unformatDateTime formatter strFixed
    )
    (unsafePartial fromJust <<< hush <<< formatDateTime formatter)
    CA.string
  where
  formatter :: String
  formatter = "YYYY-MM-DDTHH:mm:ss.SSSZ"

ed25519KeyHashCodec :: CA.JsonCodec Ed25519KeyHash
ed25519KeyHashCodec = asCborCodec "Ed25519KeyHash"

logLevelCodec :: CA.JsonCodec LogLevel
logLevelCodec =
  CA.prismaticCodec "LogLevel" readLogLevel printLogLevel
    CA.string
  where
  readLogLevel :: String -> Maybe LogLevel
  readLogLevel = case _ of
    "trace" -> Just Trace
    "debug" -> Just Debug
    "info" -> Just Info
    "warn" -> Just Warn
    "error" -> Just Error
    _ -> Nothing

  printLogLevel :: LogLevel -> String
  printLogLevel = case _ of
    Trace -> "trace"
    Debug -> "debug"
    Info -> "info"
    Warn -> "warn"
    Error -> "error"

orefCodec :: CA.JsonCodec TransactionInput
orefCodec =
  CA.prismaticCodec "TransactionInput" readOref printOref
    CA.string

printOref :: TransactionInput -> String
printOref (TransactionInput rec) =
  cborBytesToHex (encodeCbor rec.transactionId) <> "#" <> UInt.toString rec.index

publicKeyCodec :: CA.JsonCodec PublicKey
publicKeyCodec =
  CA.prismaticCodec "PublicKey" PublicKey.fromRawBytes PublicKey.toRawBytes
    rawBytesCodec

rawBytesCodec :: CA.JsonCodec RawBytes
rawBytesCodec = wrapIso RawBytes byteArrayCodec

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

txHashCodec :: CA.JsonCodec TransactionHash
txHashCodec = asCborCodec "TransactionHash"
