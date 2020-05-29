{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-coroutines"
  , "console"
  , "effect"
  , "halogen"
  , "halogen-bootstrap4"
  , "psci-support"
  , "routing"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
