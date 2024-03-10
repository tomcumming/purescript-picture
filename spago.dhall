{ name = "pictures"
, dependencies = 
  [ "effect"
  , "prelude"
  , "transformers"
  , "unsafe-reference"
  , "arrays"
  , "maybe"
  , "numbers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
