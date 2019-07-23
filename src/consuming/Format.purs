module TreeSitterNix.Format where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Traversable (foldMap, intercalate, traverse)
import Motsunabe (Doc(..), pretty)
import TreeSitterNix as Nix
import TreeSitterNix.Expr (Expr(..))
import Unsafe.Coerce (unsafeCoerce)

-- extra contextual information for generating Doc from Expr
type Context =
  -- are we inside of a fetch application? i.e. we don't need double newlines for fetch properties
  { fetch :: Boolean
  }

defaultContext :: Context
defaultContext =
  { fetch: false
  }

expr2Doc :: Context -> Expr -> Doc
expr2Doc ctx (Ellipses s) = DText s
expr2Doc ctx (Comment str) = DText str
expr2Doc ctx (Identifier str) = DText str
expr2Doc ctx (Spath str) = DText str
expr2Doc ctx (Path str) = DText str
expr2Doc ctx (Integer str) = DText str
expr2Doc ctx (Attrpath exprs) = dwords $ expr2Doc ctx <$> exprs
expr2Doc ctx (StringValue str) = DText str
expr2Doc ctx (IndentedString str) = DText str
expr2Doc ctx (Unary exprs) = expr2Doc ctx expr
expr2Doc ctx (Binary x sign y) = expr2Doc ctx x <> DText (" " <> sign <> " ") <> expr2Doc ctx y
expr2Doc ctx (Expression exprs) = dlines $ expr2Doc ctx <$> exprs
expr2Doc ctx (List exprs) = left <> choices <> right
  where
    inners = expr2Doc ctx <$> exprs
    left = DText "["
    right = DText "]"
    choices = DAlt oneLine asLines
    oneLine = dwords inners <> DText " "
    asLines = (DNest 1 (dlines inners)) <> DLine
expr2Doc ctx (Attrs exprs)
  | docs <- expr2Doc ctx <$> exprs = DAlt
  (intercalate (DText " ") docs)
  (DNest 1 (dlines docs))
expr2Doc ctx (Attrset exprs) = if Array.null exprs
  then DText "{}"
  else do
    let dlinesN = if ctx.fetch then dlines else dlines2
    let left = DText "{"
    let right = DLine <> DText "}"
    let inners = dlinesN $ expr2Doc ctx <$> exprs
    left <> DNest 1 inners <> right
expr2Doc ctx (RecAttrset exprs) = DText "rec " <> expr2Doc ctx (Attrset exprs)
expr2Doc ctx (Function input output) = input_ <> DText ": " <> output_
  where
    input_ = expr2Doc ctx input
    output_ = expr2Doc ctx output
expr2Doc ctx (Let binds expr) = let_ <> binds' <> in_ <> expr'
  where
    let_ = DText "let"
    in_ = DLine <> DLine <> DText "in "
    binds' = DNest 1 $ dlines2 $ expr2Doc ctx <$> binds
    expr'
      | Array.length expr == 1
      , Just head <- Array.head expr = expr2Doc ctx head
      | otherwise = DNest 1 $ dlines2 $ expr2Doc ctx <$> expr
expr2Doc ctx (If cond first second) = if_ <> then_ <> else_
  where
    if_ = DText "if " <> expr2Doc ctx cond
    then_ = DNest 1 $ DLine <> (DText "then ") <> expr2Doc ctx first
    else_ = DNest 1 $ DLine <> (DText "else ") <> expr2Doc ctx second
expr2Doc ctx (Parenthesized expr) = DText "(" <> expr2Doc ctx expr <> DText ")"
expr2Doc ctx (Bind name value) =
  expr2Doc ctx name <> DText " = " <> expr2Doc ctx value <> DText ";"
expr2Doc ctx (Inherit exprs) = DText "inherit" <> inner <> DText ";"
  where
    inner = dwords $ expr2Doc ctx <$> exprs
expr2Doc ctx (With name value) = DText "with " <> expr2Doc ctx name <> DText "; " <> expr2Doc ctx value
expr2Doc ctx (App fn arg) = expr2Doc ctx fn <> DText " " <> expr2Doc newCtx arg
  where
    newCtx = if containsFetch fn
      then ctx { fetch = true }
      else ctx
expr2Doc ctx (Formals exprs) = DAlt oneLine lines
  where
    exprs' = expr2Doc ctx <$> exprs
    oneLine = DText " " <> intercalate (DText ", ") exprs' <> DText " "
    lines = DNest 1 (DLine <> intercalate (DText "," <> DLine) exprs') <> DLine
expr2Doc ctx (Formal identifier Nothing) = expr2Doc ctx identifier
expr2Doc ctx (Formal identifier (Just value)) = expr2Doc ctx identifier <> DText " ? " <> expr2Doc ctx value
expr2Doc ctx (Select value selector) = expr2Doc ctx value <> DText "." <> expr2Doc ctx selector
expr2Doc ctx (Uri str) = DText str

containsFetch :: Expr -> Boolean
containsFetch (Identifier x) = String.contains (String.Pattern "fetch") x
containsFetch (Attrpath xs) = Array.any containsFetch xs
containsFetch (Select xs) = Array.any containsFetch xs
containsFetch _ = false

dwords :: forall f. Foldable f => f Doc -> Doc
dwords xs = foldMap (\x -> DText " " <> x) xs

dlines :: forall f. Foldable f => f Doc -> Doc
dlines xs = foldMap (\x -> DLine <> x) xs

dlines2 :: forall f. Foldable f => f Doc -> Doc
dlines2 xs = DLine <> intercalate (DLine <> DLine) xs

printExpr :: Expr -> String
printExpr = pretty 80 <<< expr2Doc defaultContext
