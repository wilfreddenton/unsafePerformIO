module Spec.Markdown (markdownSpec) where

import CMark.Extended (extractMetaDescription, linkifyHeaders, renderMarkdown)
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
        "![img](imgur.com)\n```\naaa\nbbb\n```\n`foo`\n\n* something\n * something1\n\n## header\nbar"
        `shouldReturn` "foo bar"
  describe "Meta Description Truncation" $ do
    it "returns without broken word" $
      pure (extractMetaDescription 6 "foo bar") `shouldReturn` "foo..."
  describe "Markdown to HTML" $ do
    let runRenderMarkdown = pure . renderMarkdown
    it "returns raw html" $
      runRenderMarkdown "<video></video>" `shouldReturn` "<p><video></video></p>\n"
  describe "Linkify Headers" $ do
    let runLinkifyHeaders = pure . linkifyHeaders
    it "returns unmodified" $
      runLinkifyHeaders "# Foo" `shouldReturn` "# Foo"
    it "returns modified" $
      runLinkifyHeaders "## Foo"
        `shouldReturn` "## <a class=\"h-anchor\" id=\"foo\" href=\"#foo\">#</a>Foo"
    it "returns modified" $
      runLinkifyHeaders "### Foo"
        `shouldReturn` "### <a class=\"h-anchor\" id=\"foo\" href=\"#foo\">#</a>Foo"
    it "returns modified" $
      runLinkifyHeaders "## [Foo]"
        `shouldReturn` "## <a class=\"h-anchor\" id=\"foo\" href=\"#foo\">#</a>[Foo]"
    it "returns modified" $
      runLinkifyHeaders "## [Foo](https://foo.com)"
        `shouldReturn` "## <a class=\"h-anchor\" id=\"foo\" href=\"#foo\">#</a>[Foo](https://foo.com)"
