module HydraSdk.Internal.Types.HeadStatus
  ( HydraHeadStatus
      ( HeadStatus_Unknown
      , HeadStatus_Idle
      , HeadStatus_Initializing
      , HeadStatus_Open
      , HeadStatus_Closed
      , HeadStatus_FanoutPossible
      , HeadStatus_Final
      )
  , headStatusCodec
  , isHeadClosed
  , printHeadStatus
  , readHeadStatus
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, prismaticCodec, string) as CA
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just))
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data HydraHeadStatus
  = HeadStatus_Unknown
  | HeadStatus_Idle
  | HeadStatus_Initializing
  | HeadStatus_Open
  | HeadStatus_Closed
  | HeadStatus_FanoutPossible
  | HeadStatus_Final

isHeadClosed :: HydraHeadStatus -> Boolean
isHeadClosed status =
  (status == HeadStatus_Closed)
    || (status == HeadStatus_FanoutPossible)
    || (status == HeadStatus_Final)

derive instance Generic HydraHeadStatus _
derive instance Eq HydraHeadStatus
derive instance Ord HydraHeadStatus

instance Show HydraHeadStatus where
  show = genericShow

instance Arbitrary HydraHeadStatus where
  arbitrary = genericArbitrary

headStatusCodec :: CA.JsonCodec HydraHeadStatus
headStatusCodec =
  CA.prismaticCodec "HydraHeadStatus" (Just <<< readHeadStatus) printHeadStatus
    CA.string

readHeadStatus :: String -> HydraHeadStatus
readHeadStatus = case _ of
  "Idle" -> HeadStatus_Idle
  "Initializing" -> HeadStatus_Initializing
  "Open" -> HeadStatus_Open
  "Closed" -> HeadStatus_Closed
  "FanoutPossible" -> HeadStatus_FanoutPossible
  "Final" -> HeadStatus_Final
  _ -> HeadStatus_Unknown

printHeadStatus :: HydraHeadStatus -> String
printHeadStatus = case _ of
  HeadStatus_Idle -> "Idle"
  HeadStatus_Initializing -> "Initializing"
  HeadStatus_Open -> "Open"
  HeadStatus_Closed -> "Closed"
  HeadStatus_FanoutPossible -> "FanoutPossible"
  HeadStatus_Final -> "Final"
  HeadStatus_Unknown -> "Unknown"
