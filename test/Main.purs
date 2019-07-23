module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import TreeSitterNix.Expr as Expr
import TreeSitterNix as Nix
import Unsafe.Coerce (unsafeCoerce)

foreign import exit :: Int -> Effect Unit

main :: Effect Unit
main = Aff.launchAff_ do
  input <- readTextFile UTF8 "test/fetch-github.nix"
  let (node :: Nix.Node) = Nix.rootNode $ Nix.parse input
  let result = Expr.parse node

  let exit' = liftEffect <<< exit

  case result of
    Right x -> do
      log "success:"
      log $ unsafeCoerce node
      log $ unsafeCoerce result
      exit' 0
    Left e -> do
      log "failure:"
      log $ unsafeCoerce e
      exit' 1
