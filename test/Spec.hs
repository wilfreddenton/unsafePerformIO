module Main where

import Protolude
import Spec.Auth (authSpec)
import Spec.Server (serverSpec)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  authTree <- testSpec "Authentication Spec" authSpec
  serverTree <- testSpec "Server Spec" serverSpec
  defaultMain $ testGroup "Specs" [serverTree, authTree]
