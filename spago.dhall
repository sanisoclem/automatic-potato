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
  , "affjax"
  , "affjax-web"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
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
  , "foreign"
  , "foreign-object"
  , "formatters"
  , "halogen"
  , "halogen-store"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "now"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "routing"
  , "routing-duplex"
  , "safe-coerce"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "uuid"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
