module Spec.Auth (
  authSpec
) where

import           Protolude
import           Test.Tasty.Hspec (Spec, describe, it, shouldReturn)

authSpec :: Spec
authSpec = do
  describe "Auth" $
    it "returns blah" $
      pure (1 :: Int) `shouldReturn` (1 :: Int)
