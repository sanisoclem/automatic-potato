{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "automatic-potato"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "argonaut"
  , "arrays"
  , "bifunctors"
  , "console"
  , "const"
  , "control"
  , "datetime"
  , "debug"
  , "decimals"
  , "effect"
  , "either"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "foreign-object"
  , "foreign"
  , "formatters"
  , "halogen-store"
  , "halogen"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "now"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "routing-duplex"
  , "routing"
  , "safe-coerce"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
