module HydraSdk.Internal.Lib.ConstrName
  ( class ConstrNames
  , class ToConstrName
  , getConstrNames
  , getConstrNames'
  , toConstrName
  , toConstrName'
  ) where

import Prelude

import Data.Array (singleton) as Array
import Data.Generic.Rep (Constructor, Sum(Inl, Inr), from) as Generic
import Data.Generic.Rep (class Generic)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(Proxy))

class ToConstrName t where
  toConstrName' :: t -> String

instance IsSymbol name => ToConstrName (Generic.Constructor name _args) where
  toConstrName' _ = reflectSymbol (Proxy :: _ name)

instance (ToConstrName a, ToConstrName b) => ToConstrName (Generic.Sum a b) where
  toConstrName' =
    case _ of
      Generic.Inl a -> toConstrName' a
      Generic.Inr b -> toConstrName' b

toConstrName :: forall a rep. Generic a rep => ToConstrName rep => a -> String
toConstrName = toConstrName' <<< Generic.from

--

class ConstrNames t where
  getConstrNames' :: Proxy t -> Array String

instance IsSymbol name => ConstrNames (Generic.Constructor name _args) where
  getConstrNames' _ = Array.singleton $ reflectSymbol (Proxy :: _ name)

instance (ConstrNames a, ConstrNames b) => ConstrNames (Generic.Sum a b) where
  getConstrNames' _ = getConstrNames' (Proxy :: _ a) <> getConstrNames' (Proxy :: _ b)

getConstrNames :: forall a rep. Generic a rep => ConstrNames rep => Proxy a -> Array String
getConstrNames _ = getConstrNames' (Proxy :: _ rep)
