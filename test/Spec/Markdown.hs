module Spec.Markdown (markdownSpec) where

import CMark.Extended (extractMetaDescription, linkifyHeaders, reifyFootnotes, renderMarkdown)
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
  describe "Reify Footnotes" $ do
    let runReifyFootnotes = pure . reifyFootnotes
    it "returns unmodified" $
      runReifyFootnotes "" `shouldReturn` ""
    it "returns unmodified" $
      runReifyFootnotes "foo" `shouldReturn` "foo"
    it "returns unmodified" $
      runReifyFootnotes "foo\n\n" `shouldReturn` "foo\n\n"
    it "returns with empty footnote block" $
      -- must be prefixed with \n\n (block)
      -- without references generated footnote block is empty
      runReifyFootnotes "[^foo]: hey\n\n[^bar]: you"
        `shouldReturn` "[^foo]: hey\n\n---\n\n<ol></ol>"
    it "returns with empty footnote block" $
      runReifyFootnotes "\n\n[^foo]: hey\n\n[^bar]: you"
        `shouldReturn` "\n\n---\n\n<ol></ol>"
    it "returns with correct footnote block" $
      runReifyFootnotes "foo.[^foo]\n\nbar.[^bar]\n\n[^foo]: hey\n\n[^bar]: you"
        `shouldReturn` "foo.<sup><a id=\"fn-1\" href=\"#fnr-1\">1</a></sup>\n\nbar.<sup><a id=\"fn-2\" href=\"#fnr-2\">2</a></sup>\n\n---\n\n<ol><li id=\"fnr-1\"><p>hey <a class=\"rfn\" href=\"#fn-1\">\8617</a></p>\n</li>\n<li id=\"fnr-2\"><p>you <a class=\"rfn\" href=\"#fn-2\">\8617</a></p>\n</li>\n</ol>"
    it "returns with correct footnote block" $
      runReifyFootnotes "foo.[^foo]\n\n[^foo]: hey\n\n[^bar]: you\n\nbar.[^bar]\n\n"
        `shouldReturn` "foo.<sup><a id=\"fn-1\" href=\"#fnr-1\">1</a></sup>\n\nbar.<sup><a id=\"fn-2\" href=\"#fnr-2\">2</a></sup>\n\n---\n\n<ol><li id=\"fnr-1\"><p>hey <a class=\"rfn\" href=\"#fn-1\">\8617</a></p>\n</li>\n<li id=\"fnr-2\"><p>you <a class=\"rfn\" href=\"#fn-2\">\8617</a></p>\n</li>\n</ol>"
