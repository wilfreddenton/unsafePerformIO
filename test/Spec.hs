module Main where

import Protolude
import Spec.Auth (authSpec)
import Spec.Markdown (markdownSpec)
import Spec.Server (serverSpec)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  authTree <- testSpec "Authentication Spec" authSpec
  markdownTree <- testSpec "Markdown Spec" markdownSpec
  serverTree <- testSpec "Server Spec" serverSpec
  defaultMain $ testGroup "Specs" [markdownTree, serverTree, authTree]
