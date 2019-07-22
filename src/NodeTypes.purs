module TreeSitterNode.Nodetypes where

import Data.Maybe (Maybe)
import Simple.JSON as JSON

type Children =
  { multiple :: Boolean
  , required :: Boolean
  , types :: Array NodeReference
  }

type Node =
  { "type" :: String
  , named :: Boolean
  , children :: Maybe Children
  }

type NodeReference =
  { "type" :: String
  , named :: Boolean
  }

readNodes :: String -> JSON.E (Array Node)
readNodes json =
  JSON.readJSON json
