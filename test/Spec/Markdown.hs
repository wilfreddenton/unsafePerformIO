module Spec.Markdown (markdownSpec) where

import Lucid.Extended (extractMetaDescription, renderMarkdown)
import Protolude
import Test.Tasty.Hspec (Spec, describe, it, shouldReturn)

markdownSpec :: Spec
markdownSpec = do
  describe "Meta Description Extraction" $ do
    let runExtractMetaDescription = pure . extractMetaDescription 100
    it "returns stripped" $
      runExtractMetaDescription " foo bar " `shouldReturn` "foo bar"
    it "returns without symbols" $
      runExtractMetaDescription "foo, *foo*, **foo**, `foo`"
        `shouldReturn` "foo, foo, foo, foo"
    it "returns without symbols - nested" $
      runExtractMetaDescription "***foo***, *`foo`*, **`foo`**, `*foo*`"
        `shouldReturn` "foo, foo, foo, *foo*"
    it "returns without newlines - softbreak" $
      runExtractMetaDescription "foo.\nbar." `shouldReturn` "foo. bar."
    it "returns without newlines - paragraph" $
      runExtractMetaDescription "foo.\n\nbar." `shouldReturn` "foo. bar."
    it "returns only paragraph text" $
      runExtractMetaDescription
        "```\naaa\nbbb\n```\n`foo`\n\n* something\n * something1\n\n## header\nbar"
        `shouldReturn` "foo bar"
  describe "Meta Description Truncation" $ do
    it "returns without broken word" $
      pure (extractMetaDescription 6 "foo bar") `shouldReturn` "foo..."
  describe "Markdown to HTML" $ do
    let runRenderMarkdown = pure . renderMarkdown
    it "returns raw html" $
      runRenderMarkdown "<video></video>" `shouldReturn` "<p><video></video></p>\n"
