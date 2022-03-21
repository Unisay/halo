{ name = "halo"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "console"
  -- , "debug"
  , "effect"
  , "enums"
  , "foldable-traversable"
  , "maybe"
  , "nullable"
  , "prelude"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
