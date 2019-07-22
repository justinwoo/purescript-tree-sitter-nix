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

parse :: Nix.Node -> Either (Array ParseFailure) Expr
parse n = runExcept $ parse_ n

parse_ :: ParseNixNode Expr
parse_ n = oneOf $ map (applyFlipped n) choices
  where
    parse' = fix \_ -> parse_
    choices =
      [ parseFromSpec (ParseSpec "app" :: ParseSpec "App" ManyChildren) parse'
      , parseFromSpec (ParseSpec "assert" :: ParseSpec "Assert" ManyChildren) parse'
      , parseFromSpec (ParseSpec "attrpath" :: ParseSpec "Attrpath" ManyChildren) parse'
      , parseFromSpec (ParseSpec "attrs" :: ParseSpec "Attrs" ManyChildren) parse'
      , parseFromSpec (ParseSpec "attrset" :: ParseSpec "Attrset" ManyChildren) parse'
      , parseFromSpec (ParseSpec "binary" :: ParseSpec "Binary" ManyChildren) parse'
      , parseFromSpec (ParseSpec "bind" :: ParseSpec "Bind" ManyChildren) parse'
      , parseFromSpec (ParseSpec "binds" :: ParseSpec "Binds" ManyChildren) parse'
      , parseFromSpec (ParseSpec "expression" :: ParseSpec "Expression" ManyChildren) parse'
      , parseFromSpec (ParseSpec "formal" :: ParseSpec "Formal" ManyChildren) parse'
      , parseFromSpec (ParseSpec "formals" :: ParseSpec "Formals" ManyChildren) parse'
      , parseFromSpec (ParseSpec "function" :: ParseSpec "Function" ManyChildren) parse'
      , parseFromSpec (ParseSpec "if" :: ParseSpec "If" ManyChildren) parse'
      , parseFromSpec (ParseSpec "indented_string" :: ParseSpec "IndentedString" Childless) parse'
      , parseFromSpec (ParseSpec "inherit" :: ParseSpec "Inherit" ManyChildren) parse'
      , parseFromSpec (ParseSpec "interpolation" :: ParseSpec "Interpolation" ManyChildren) parse'
      , parseFromSpec (ParseSpec "let" :: ParseSpec "Let" ManyChildren) parse'
      , parseFromSpec (ParseSpec "let_attrset" :: ParseSpec "LetAttrset" ManyChildren) parse'
      , parseFromSpec (ParseSpec "list" :: ParseSpec "List" ManyChildren) parse'
      , parseFromSpec (ParseSpec "parenthesized" :: ParseSpec "Parenthesized" ManyChildren) parse'
      , parseFromSpec (ParseSpec "rec_attrset" :: ParseSpec "RecAttrset" ManyChildren) parse'
      , parseFromSpec (ParseSpec "select" :: ParseSpec "Select" ManyChildren) parse'
      , parseFromSpec (ParseSpec "string" :: ParseSpec "StringValue" Childless) parse'
      , parseFromSpec (ParseSpec "unary" :: ParseSpec "Unary" ManyChildren) parse'
      , parseFromSpec (ParseSpec "with" :: ParseSpec "With" ManyChildren) parse'
      , parseFromSpec (ParseSpec "identifier" :: ParseSpec "Identifier" Childless) parse'
      , parseFromSpec (ParseSpec "integer" :: ParseSpec "Integer" Childless) parse'
      , parseFromSpec (ParseSpec "float" :: ParseSpec "Float" Childless) parse'
      , parseFromSpec (ParseSpec "path" :: ParseSpec "Path" Childless) parse'
      , parseFromSpec (ParseSpec "hpath" :: ParseSpec "Hpath" Childless) parse'
      , parseFromSpec (ParseSpec "spath" :: ParseSpec "Spath" Childless) parse'
      , parseFromSpec (ParseSpec "uri" :: ParseSpec "Uri" Childless) parse'
      , parseFromSpec (ParseSpec "ellipses" :: ParseSpec "Ellipses" Childless) parse'
      , parseFromSpec (ParseSpec "comment" :: ParseSpec "Comment" Childless) parse'
      ]

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
