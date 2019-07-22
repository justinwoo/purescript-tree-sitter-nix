module Codegen where

import Prelude

import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Data.String.CodeUnits (singleton, toCharArray)
import TreeSitterNode.Nodetypes (Node)

data DatatypeKind
  = Childless
  | OneChild -- never occurs in tree-sitter-nix as of jul 2019
  | ManyChildren

data Datatype =
  Datatype
    DatatypeKind
    { name :: String
    , typeName :: String
    }

mkDatatype :: Node -> Maybe Datatype
mkDatatype { named: false } = Nothing
mkDatatype { "type": "string" } = Just $ Datatype Childless
  { name: "StringValue", typeName: "string" }
mkDatatype { "type": "indented_string" } = Just $ Datatype Childless
  { name: "IndentedString", typeName: "indented_string" }
mkDatatype r = Just (Datatype k r')
  where
    r' = { name: renameToCamelCase r."type", typeName: r."type" }
    k = case r.children of
      Nothing -> Childless
      Just c | not c.multiple -> OneChild
      Just c -> ManyChildren

printDatatypes :: Array Datatype -> String
printDatatypes xs = "data Expr" <> "\n  = " <> Array.intercalate "\n  | " (print <$> xs)
  where
    print (Datatype k r) = case k of
      Childless -> r.name <> " String"
      OneChild -> r.name <> " Expr"
      ManyChildren -> r.name <> " (Array Expr)"

printParseSpecs :: Array Datatype -> String
printParseSpecs xs = lines <> choices
  where
    lines = Array.intercalate "\n"
      [ "parse :: Nix.Node -> Either (Array ParseFailure) Expr"
      , "parse n = runExcept $ parse_ n"
      , ""
      , "parse_ :: ParseNixNode Expr"
      , "parse_ n = oneOf $ map (applyFlipped n) choices"
      , "  where"
      , "    parse' = fix \\_ -> parse_"
      ]

    choices = "\n    choices =\n      [ "
      <> Array.intercalate "\n      , " (print <$> xs)
      <> "\n      ]"

    printChildKind k = case k of
      Childless -> " Childless (String -> Expr))"
      OneChild -> " OneChild (Expr -> Expr))"
      ManyChildren -> " ManyChildren (Array Expr -> Expr))"

    print (Datatype k r) = do
      Array.intercalate " "
        [ "parseFromSpec"
        , "(ParseSpec"
        , r.name
        , quoted r.typeName
        , "::"
        , "ParseSpec"
        , quoted r.name
        , printChildKind k
        , "parse'"
        ]

quoted :: String -> String
quoted s = "\"" <> s <> "\""

renameToCamelCase :: String -> String
renameToCamelCase str = result
  where
    list = List.fromFoldable $ map singleton $ toCharArray str
    result = case list of
      x : rest -> toUpper x <> change rest
      Nil -> ""
    change xs = case xs of
      "_" : x : rest -> toUpper x <> change rest
      x : rest -> x <> change rest
      Nil -> ""
