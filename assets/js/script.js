function main() {
  // code
  document.querySelectorAll('pre code').forEach(block => {
    hljs.highlightBlock(block)
    hljs.lineNumbersBlock(block)
  })

  // math
  renderMathInElement(document.body)
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main)
} else {
  main()
}
