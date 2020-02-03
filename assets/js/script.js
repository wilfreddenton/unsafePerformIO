function main() {
  // code
  document.querySelectorAll('pre code').forEach(block => {
    hljs.highlightBlock(block)
  })

  // math
  renderMathInElement(document.body)

  // videos
  document.querySelectorAll('video').forEach(async v => {
    v.addEventListener('pause', () => {
      if (v.autoplay) {
        v.play()
      }
    })
  })
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main)
} else {
  main()
}
