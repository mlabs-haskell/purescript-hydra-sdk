module HydraSdk.Internal.Http.Error
  ( AffjaxError(AffjaxError)
  , ArgonautJson(ArgonautJson)
  , HttpError(DecodeJsonError, HttpRequestError, HttpResponseError)
  ) where

import Prelude

import Affjax (Error, printError) as Affjax
import Affjax.StatusCode (StatusCode) as Affjax
import Data.Argonaut (Json, stringify)
import Data.Codec.Argonaut (JsonDecodeError) as CA
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)

data HttpError
  = DecodeJsonError String CA.JsonDecodeError
  | HttpRequestError AffjaxError
  | HttpResponseError Affjax.StatusCode String

derive instance Generic HttpError _

instance Show HttpError where
  show = genericShow

newtype AffjaxError = AffjaxError Affjax.Error

derive instance Newtype AffjaxError _

instance Show AffjaxError where
  show = Affjax.printError <<< unwrap

newtype ArgonautJson = ArgonautJson Json

derive instance Newtype ArgonautJson _
derive instance Eq ArgonautJson

instance Show ArgonautJson where
  show = stringify <<< unwrap
