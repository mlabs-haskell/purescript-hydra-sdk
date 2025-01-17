{ name = "hydra-sdk-example-minimal"
, dependencies =
  [ "aeson"
  , "aff"
  , "arrays"
  , "avar"
  , "cardano-transaction-lib"
  , "cardano-types"
  , "codec-aeson"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "hydra-sdk"
  , "maybe"
  , "monad-logger"
  , "newtype"
  , "node-child-process"
  , "node-process"
  , "ordered-collections"
  , "posix-types"
  , "prelude"
  , "profunctor-lenses"
  , "quickcheck"
  , "refs"
  , "transformers"
  , "tuples"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "app/**/*.purs" ]
}
