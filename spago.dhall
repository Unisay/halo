{ name = "halo"
, dependencies =
  [ "aff"
  , "ansi"
  , "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "node-process"
  , "node-streams"
  , "nullable"
  , "prelude"
  , "strings"
  , "tailrec"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
