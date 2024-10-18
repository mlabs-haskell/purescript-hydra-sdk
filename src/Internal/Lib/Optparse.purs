module HydraSdk.Internal.Lib.Optparse
  ( parserReader
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Options.Applicative (ReadM, eitherReader) as Optparse
import Parsing (Parser, runParser)

parserReader :: forall a. String -> Parser String a -> Optparse.ReadM a
parserReader typeName parser =
  Optparse.eitherReader $ \str ->
    runParser str parser # lmap \err ->
      "Cannot parse as "
        <> typeName
        <> ". Input string: \""
        <> str
        <> "\", error: "
        <> show err
