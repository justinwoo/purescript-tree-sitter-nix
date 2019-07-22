module TreeSitterNode.ParseNode where

import Prelude

import Control.Monad.Except (Except, throwError)
import Data.Traversable (traverse)
import TreeSitterNix as Nix

foreign import kind ChildKind
foreign import data Childless :: ChildKind
foreign import data ManyChildren :: ChildKind

newtype ParseFailure = ParseFailure String
type ParseResult a = Except (Array ParseFailure) a

type TypeName = String
data ParseSpec (name :: Symbol) (k :: ChildKind) ctr = ParseSpec ctr TypeName
type ParseNixNode result = Nix.Node -> ParseResult result

class ParseFromSpec (name :: Symbol) (k :: ChildKind) ctr result
  | k name -> ctr result where
  parseFromSpec :: ParseSpec name k ctr -> ParseNixNode result -> ParseNixNode result

instance parseFromSpecChildLess :: ParseFromSpec name Childless (String -> result) result where
  parseFromSpec (ParseSpec ctr typeName) _ n =
    if _typeName /= typeName
      then throwError
        [ ParseFailure $ "Could not match type " <> _typeName <> " with expected " <> typeName ]
      else pure $ ctr $ Nix.text n
    where
      Nix.TypeString _typeName = Nix.type_ n

instance parseFromSpecManyChildren :: ParseFromSpec name ManyChildren (Array result -> result) result where
  parseFromSpec (ParseSpec ctr typeName) parseInner n =
    if _typeName /= typeName
      then throwError
        [ ParseFailure $ "Could not match type " <> _typeName <> " with expected " <> typeName ]
      else do
        map ctr $ traverse parseInner $ Nix.namedChildren n
    where
      Nix.TypeString _typeName = Nix.type_ n
