{ name = "hydra-sdk"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "bytearrays"
  , "cardano-transaction-lib"
  , "cardano-types"
  , "codec-argonaut"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "errors"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "formatters"
  , "http-methods"
  , "integers"
  , "js-bigints"
  , "js-date"
  , "maybe"
  , "monad-logger"
  , "newtype"
  , "node-buffer"
  , "node-child-process"
  , "node-fs"
  , "node-path"
  , "node-streams"
  , "optparse"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "plutus-types"
  , "prelude"
  , "profunctor"
  , "quickcheck"
  , "safely"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "uint"
  , "uri"
  , "uuid"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
