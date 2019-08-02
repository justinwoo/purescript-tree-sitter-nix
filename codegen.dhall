let base = ./spago.dhall

in      base
    //  { sources =
            [ "codegen/*.purs", "src/*.purs" ]
        , dependencies =
            base.dependencies # [ "console", "effect", "node-fs-aff" ]
        }
