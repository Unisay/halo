{ name = "halo"
, dependencies =
  [ "console"
  , "dodo-printer"
  , "effect"
  , "prelude"
  -- , "purescript-language-cst-parser"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
