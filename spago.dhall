{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "tree-sitter-nix"
, dependencies =
    [ "console"
    , "effect"
    , "generics-rep"
    , "motsunabe"
    , "node-fs-aff"
    , "simple-json"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "codegen/**/*.purs", "src/**/*.purs", "test/**/*.purs" ]
}
