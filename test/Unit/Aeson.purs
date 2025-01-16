module Test.Unit.Aeson
  ( suite
  ) where

import Prelude

import Aeson (Aeson, decodeAeson, getField, parseJsonStringToAeson, printJsonDecodeError)
import Control.Error.Util (bool)
import Control.Monad.Error.Class (liftEither, liftMaybe)
import Data.Array (elem) as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (decode) as CA
import Data.Either (isRight)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import HydraSdk.Internal.Lib.ConstrName (getConstrNames)
import HydraSdk.Internal.Lib.Misc (concatPathSegments)
import HydraSdk.Types (HydraNodeApi_InMessage, hydraNodeApiInMessageCodec)
import Mote (group, skip, test)
import Mote.TestPlanM (TestPlanM)
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.FS.Sync (readTextFile)
import Node.Process (lookupEnv)
import Test.Spec.Assertions (shouldSatisfy)
import Type.Proxy (Proxy(Proxy))

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Aeson" do
    group "HydraNodeApi_InMessage" do
      -- hydraNodeApiInMessageTest "Greetings"
      traverse_ hydraNodeApiInMessageTest $
        getConstrNames (Proxy :: _ HydraNodeApi_InMessage)

hydraNodeApiInMessageTest :: String -> TestPlanM (Aff Unit) Unit
hydraNodeApiInMessageTest constrName =
  bool identity skip (Array.elem constrName ignoredTestcases) do
    test constrName do
      fixtures <- liftEffect $ readHydraFixtures constrName
      -- traceM $ "fixtures: " <> show fixtures
      traverse_ (flip shouldSatisfy isRight <<< CA.decode hydraNodeApiInMessageCodec)
        fixtures

readHydraFixtures :: String -> Effect (Array Aeson)
readHydraFixtures constrName = do
  hydraFixturesDir <-
    liftMaybe (error "readHydraFixture: HYDRA_FIXTURES environment variable not set")
      =<< lookupEnv "HYDRA_FIXTURES"
  let fp = concatPathSegments hydraFixturesDir $ constrName <> ".json"
  contents <- readTextFile Encoding.UTF8 fp
  liftEither $ (flip getField "samples" =<< decodeAeson =<< parseJsonStringToAeson contents) #
    lmap
      ( \err -> error $ "readHydraFixture: Could not load hydra-node fixture for "
          <> constrName
          <> ", error: "
          <> printJsonDecodeError err
      )

ignoredTestcases :: Array String
ignoredTestcases =
  [ "SnapshotConfirmed" -- Unsupported format
  ]
