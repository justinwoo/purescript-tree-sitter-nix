module Main where

import Prelude

import Codegen (mkDatatype, printDatatypes, printParseSpecs)
import Data.Array as Array
import Data.Either (either)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import TreeSitterNode.Nodetypes (readNodes)

main :: Effect Unit
main = Aff.launchAff_ do
  json <- readTextFile UTF8 "node-types.json"
  nodes <- either mempty pure $ readNodes json
  let datatypes = Array.mapMaybe mkDatatype nodes
  let expr = printDatatypes datatypes
  let parse = printParseSpecs datatypes
  let output = template { expr, parse }
  let path = "test/Expr.purs"

  writeTextFile UTF8 path output

  log $ "wrote to " <> path

template :: { expr :: String, parse :: String } -> String
template { expr, parse } = """module Test.Expr where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Foldable (oneOf)
import Data.Function (applyFlipped)
import Data.Generic.Rep (class Generic)
import TreeSitterNix as Nix
import TreeSitterNode.ParseNode (Childless, ManyChildren, ParseFailure, ParseNixNode, ParseSpec(..), parseFromSpec)

""" <> expr <> """
derive instance genericExpr :: Generic Expr _

""" <> parse <> """
"""
