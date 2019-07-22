module Main where

import Prelude

import Codegen (mkDatatype, printDatatypes, printParseSpecs)
import Data.Array as Array
import Data.Either (either)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import TreeSitterNode.Nodetypes (readNodes)

main :: Effect Unit
main = Aff.launchAff_ do
  json <- readTextFile UTF8 "node-types.json"
  nodes <- either mempty pure $ readNodes json
  let datatypes = Array.mapMaybe mkDatatype nodes

  log ""
  log $ printDatatypes datatypes
  log ""
  log $ printParseSpecs datatypes
