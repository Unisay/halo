{ name = "halo"
, dependencies =
  [ "aff"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "maybe"
  , "node-buffer"
  , "node-process"
  , "node-streams"
  , "nullable"
  , "prelude"
  , "strings"
  , "tailrec"
  , "transformers"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
