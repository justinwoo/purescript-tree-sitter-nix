{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "tree-sitter-nix"
, dependencies =
    [ "generics-rep", "simple-json" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
