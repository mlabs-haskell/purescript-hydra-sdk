let upstream =
    -- https://github.com/mlabs-haskell/purescript-cardano-package-set
      https://raw.githubusercontent.com/mlabs-haskell/purescript-cardano-package-set/v3.1.0/packages.dhall
        sha256:0d8a7ca4e8ecfc8d1d795a989b76364caa9583d60e765c490cfa215a8824c246

let additions =
      { aeson =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "const"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "js-bigints"
          , "lists"
          , "maybe"
          , "mote"
          , "numbers"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "record"
          , "spec"
          , "strings"
          , "tuples"
          , "typelevel"
          , "typelevel-prelude"
          , "uint"
          , "untagged-union"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-aeson.git"
        , version = "e7bfc24a941e1d73de88de3f4a034e1da33cc8ae"
        }
      , cardano-data-lite =
        { dependencies =
          [ "aeson"
          , "aff"
          , "argonaut"
          , "bifunctors"
          , "bytearrays"
          , "effect"
          , "either"
          , "enums"
          , "maybe"
          , "newtype"
          , "nullable"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "profunctor"
          , "spec"
          , "transformers"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-cardano-data-lite"
        , version = "44a08930844221afc4ef750104940f81a75b996e"
        } 
      , codec-aeson =
        { dependencies =
          [ "aeson"
          , "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "codec"
          , "console"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "gen"
          , "integers"
          , "lists"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "profunctor"
          , "quickcheck"
          , "record"
          , "safe-coerce"
          , "strings"
          , "transformers"
          , "tuples"
          , "type-equality"
          , "typelevel-prelude"
          , "unsafe-coerce"
          , "variant"
          ]
        , repo = "https://github.com/errfrom/purescript-codec-argonaut.git"
        , version = "59996776e733a1e0f1daf085f153b2cca8527907"
        }
      , errors =
        { dependencies =
          [ "control"
          , "effect"
          , "either"
          , "identity"
          , "maybe"
          , "newtype"
          , "prelude"
          , "test-unit"
          , "transformers"
          ]
        , repo = "https://github.com/passy/purescript-errors.git"
        , version = "670485beb1e026f77d52ca58ce10c145d96c11ba"
        }
      }

in (upstream // additions)
