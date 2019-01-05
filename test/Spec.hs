module Spec where

import           Protolude
import           Spec.Server      (serverSpecs)
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  serverTrees <- traverse (uncurry testSpec) serverSpecs
  defaultMain $ testGroup "specs" serverTrees
