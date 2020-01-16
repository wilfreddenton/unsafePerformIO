function main() {
  document.querySelectorAll('pre').forEach(pre => {
    const code = pre.querySelector('code')
    if (code) {
      const frag = document.createDocumentFragment()
      const lines = code.innerHTML.trim().split('\n')
      lines.forEach(line => {
        const el = document.createElement('code')
        el.innerHTML = line
        frag.append(el)
      })
      pre.removeChild(code)
      pre.appendChild(frag)
    }
  })
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main)
} else {
  main()
}
