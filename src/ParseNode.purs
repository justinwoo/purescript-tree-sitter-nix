module TreeSitterNode.ParseNode where

import Prelude

import Control.Monad.Except (Except, throwError)
import Data.Generic.Rep (class Generic)
import Data.Traversable (traverse)
import Data.Variant as Variant
import TreeSitterNix as Nix
import Kishimen (class GenericSumToVariant, variantToGenericSum)
import Prim.Row as Row
import Type.Prelude (class IsSymbol, SProxy(..))

foreign import kind ChildKind
foreign import data Childless :: ChildKind
foreign import data ManyChildren :: ChildKind

newtype ParseFailure = ParseFailure String
type ParseResult a = Except (Array ParseFailure) a

type TypeName = String
data ParseSpec (name :: Symbol) (k :: ChildKind) = ParseSpec TypeName
type ParseNixNode result = Nix.Node -> ParseResult result

class ParseFromSpec (name :: Symbol) (k :: ChildKind) result
  | k name -> result where
  parseFromSpec :: ParseSpec name k -> ParseNixNode result -> ParseNixNode result

instance parseFromSpecChildLess ::
  ( GenericSumToVariant rep row
  , Generic result rep
  , IsSymbol name
  , Row.Cons name String row' row
  ) => ParseFromSpec name Childless result where
  parseFromSpec (ParseSpec typeName) _ n =
    if _typeName /= typeName
      then throwError
        [ ParseFailure $ "Could not match type " <> _typeName <> " with expected " <> typeName ]
      else pure $ variantToGenericSum $ Variant.inj (SProxy :: _ name) $ Nix.text n
    where
      Nix.TypeString _typeName = Nix.type_ n

instance parseFromSpecManyChildren ::
  ( GenericSumToVariant rep row
  , Generic result rep
  , IsSymbol name
  , Row.Cons name (Array result) row' row
  ) => ParseFromSpec name ManyChildren result where
  parseFromSpec (ParseSpec typeName) parseInner n =
    if _typeName /= typeName
      then throwError
        [ ParseFailure $ "Could not match type " <> _typeName <> " with expected " <> typeName ]
      else do
        xs <- traverse parseInner $ Nix.namedChildren n
        pure $ variantToGenericSum $ Variant.inj (SProxy :: _ name) xs
    where
      Nix.TypeString _typeName = Nix.type_ n
