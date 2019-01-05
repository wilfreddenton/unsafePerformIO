module Spec where

import           Protolude
import           Spec.Server      (serverSpec)
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  serverTree <- testSpec "Server Spec" serverSpec
  defaultMain $ testGroup "specs"
    [serverTree]
