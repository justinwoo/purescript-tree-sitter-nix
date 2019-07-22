module TreeSitterNix where

import Prelude

import Data.Array as Array
import Data.Newtype (class Newtype)
import Unsafe.Coerce (unsafeCoerce)

foreign import data TreeSitterLanguage :: Type
foreign import data TreeSitterParser :: Type

foreign import data Tree :: Type

-- | Node from tree-sitter
foreign import data Node :: Type

foreign import nixLanguage :: TreeSitterLanguage
foreign import mkParser :: TreeSitterLanguage -> TreeSitterParser

parser :: TreeSitterParser
parser = mkParser nixLanguage

parse :: String -> Tree
parse contents = parser'.parse contents
  where parser' = unsafeCoerce parser :: { parse :: String -> Tree }

rootNode :: Tree -> Node
rootNode tree = tree'.rootNode
  where tree' = unsafeCoerce tree :: { rootNode :: Node }

children :: Node -> Array Node
children tn = tn'.children
  where tn' = unsafeCoerce tn :: { children :: Array Node }

-- | Filter for named children
namedChildren :: Node -> Array Node
namedChildren = Array.filter isNamed <<< children

-- | Is a given Node Real or is it fake?
isNamed :: Node -> Boolean
isNamed tn = tn'.isNamed
  where tn' = unsafeCoerce tn :: { isNamed :: Boolean }

text :: Node -> String
text tn = tn'.text
  where tn' = unsafeCoerce tn :: { text :: String }

newtype TypeString = TypeString String
derive instance newtypeTypeString :: Newtype TypeString _
derive newtype instance eqTypeString :: Eq TypeString

type_ :: Node -> TypeString
type_ tn = tn'."type"
  where tn' = unsafeCoerce tn :: { "type" :: TypeString }

nodeToString :: Node -> String
nodeToString node = node'.toString unit
  where node' = unsafeCoerce node :: { toString :: Unit -> String }
