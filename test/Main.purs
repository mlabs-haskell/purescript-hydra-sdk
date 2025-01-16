module Test.Main where

import Prelude

import Data.Maybe (Maybe(Just))
import Data.Newtype (wrap)
import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Data.Posix.Signal (toString) as Signal
import Effect (Effect)
import Effect.Aff (Aff, Fiber, killFiber, launchAff, launchAff_)
import Effect.Exception (error)
import Mote.TestPlanM (TestPlanM, interpretWithConfig)
import Node.Process (onSignal) as Process
import Test.Spec.Runner (Config, defaultConfig)
import Test.Unit.Aeson (suite) as Aeson

suite :: TestPlanM (Aff Unit) Unit
suite = Aeson.suite

main :: Effect Unit
main = do
  fiber <- launchAff $ interpretWithConfig runnerConfig suite
  interruptOnSignal SIGINT fiber
  interruptOnSignal SIGTERM fiber

runnerConfig :: Config
runnerConfig = defaultConfig { timeout = Just $ wrap 90000.0 }

interruptOnSignal :: forall a. Signal -> Fiber a -> Effect Unit
interruptOnSignal signal fiber =
  Process.onSignal signal do
    launchAff_ do
      killFiber (error $ Signal.toString signal) fiber
