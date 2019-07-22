module Test.Expr where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Foldable (oneOf)
import Data.Function (applyFlipped)
import Data.Generic.Rep (class Generic)
import TreeSitterNix as Nix
import TreeSitterNode.ParseNode (Childless, ManyChildren, ParseFailure, ParseNixNode, ParseSpec(..), parseFromSpec)

data Expr
  = App (Array Expr)
  | Assert (Array Expr)
  | Attrpath (Array Expr)
  | Attrs (Array Expr)
  | Attrset (Array Expr)
  | Binary (Array Expr)
  | Bind (Array Expr)
  | Binds (Array Expr)
  | Expression (Array Expr)
  | Formal (Array Expr)
  | Formals (Array Expr)
  | Function (Array Expr)
  | If (Array Expr)
  | IndentedString String
  | Inherit (Array Expr)
  | Interpolation (Array Expr)
  | Let (Array Expr)
  | LetAttrset (Array Expr)
  | List (Array Expr)
  | Parenthesized (Array Expr)
  | RecAttrset (Array Expr)
  | Select (Array Expr)
  | StringValue String
  | Unary (Array Expr)
  | With (Array Expr)
  | Identifier String
  | Integer String
  | Float String
  | Path String
  | Hpath String
  | Spath String
  | Uri String
  | Ellipses String
  | Comment String
derive instance genericExpr :: Generic Expr _

parse :: Nix.Node -> Either (Array ParseFailure) Expr
parse n = runExcept $ parse_ n

parse_ :: ParseNixNode Expr
parse_ n = oneOf $ map (applyFlipped n) choices
  where
    parse' = fix \_ -> parse_
    choices =
      [ parseFromSpec (ParseSpec App "app" :: ParseSpec "App"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Assert "assert" :: ParseSpec "Assert"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Attrpath "attrpath" :: ParseSpec "Attrpath"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Attrs "attrs" :: ParseSpec "Attrs"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Attrset "attrset" :: ParseSpec "Attrset"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Binary "binary" :: ParseSpec "Binary"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Bind "bind" :: ParseSpec "Bind"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Binds "binds" :: ParseSpec "Binds"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Expression "expression" :: ParseSpec "Expression"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Formal "formal" :: ParseSpec "Formal"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Formals "formals" :: ParseSpec "Formals"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Function "function" :: ParseSpec "Function"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec If "if" :: ParseSpec "If"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec IndentedString "indented_string" :: ParseSpec "IndentedString"  Childless (String -> Expr)) parse'
      , parseFromSpec (ParseSpec Inherit "inherit" :: ParseSpec "Inherit"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Interpolation "interpolation" :: ParseSpec "Interpolation"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Let "let" :: ParseSpec "Let"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec LetAttrset "let_attrset" :: ParseSpec "LetAttrset"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec List "list" :: ParseSpec "List"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Parenthesized "parenthesized" :: ParseSpec "Parenthesized"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec RecAttrset "rec_attrset" :: ParseSpec "RecAttrset"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Select "select" :: ParseSpec "Select"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec StringValue "string" :: ParseSpec "StringValue"  Childless (String -> Expr)) parse'
      , parseFromSpec (ParseSpec Unary "unary" :: ParseSpec "Unary"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec With "with" :: ParseSpec "With"  ManyChildren (Array Expr -> Expr)) parse'
      , parseFromSpec (ParseSpec Identifier "identifier" :: ParseSpec "Identifier"  Childless (String -> Expr)) parse'
      , parseFromSpec (ParseSpec Integer "integer" :: ParseSpec "Integer"  Childless (String -> Expr)) parse'
      , parseFromSpec (ParseSpec Float "float" :: ParseSpec "Float"  Childless (String -> Expr)) parse'
      , parseFromSpec (ParseSpec Path "path" :: ParseSpec "Path"  Childless (String -> Expr)) parse'
      , parseFromSpec (ParseSpec Hpath "hpath" :: ParseSpec "Hpath"  Childless (String -> Expr)) parse'
      , parseFromSpec (ParseSpec Spath "spath" :: ParseSpec "Spath"  Childless (String -> Expr)) parse'
      , parseFromSpec (ParseSpec Uri "uri" :: ParseSpec "Uri"  Childless (String -> Expr)) parse'
      , parseFromSpec (ParseSpec Ellipses "ellipses" :: ParseSpec "Ellipses"  Childless (String -> Expr)) parse'
      , parseFromSpec (ParseSpec Comment "comment" :: ParseSpec "Comment"  Childless (String -> Expr)) parse'
      ]
