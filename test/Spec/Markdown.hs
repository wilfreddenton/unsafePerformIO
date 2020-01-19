module Spec.Markdown (markdownSpec) where

import Lucid.Extended (extractMetaDescription)
import Protolude
import Test.Tasty.Hspec (Spec, describe, it, shouldReturn)

markdownSpec :: Spec
markdownSpec = do
  describe "Meta Description Extraction" $ do
    it "returns without symbols" $
      pure (extractMetaDescription "foo, *foo*, **foo**, `foo`")
        `shouldReturn` "foo, foo, foo, foo"
    it "returns without symbols - nested" $
      pure (extractMetaDescription "***foo***, *`foo`*, **`foo`**, `*foo*`")
        `shouldReturn` "foo, foo, foo, *foo*"
    it "returns without newlines - softbreak" $
      pure (extractMetaDescription "foo\nbar") `shouldReturn` "foo bar"
    it "returns without newlines - paragraph" $
      pure (extractMetaDescription "foo\n\nbar") `shouldReturn` "foo bar"
